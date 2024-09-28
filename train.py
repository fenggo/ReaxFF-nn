#!/usr/bin/env python
# -*- coding: utf-8 -*-
import matplotlib
import sys
import argparse
from irff.reax import ReaxFF
from irff.mpnn import MPNN
from irff.data.ColData import ColData


parser = argparse.ArgumentParser(description='nohup ./train.py --v=1 --h=0> py.log 2>&1 &')
parser.add_argument('--step',default=10000,type=int, help='training steps')
parser.add_argument('--lr',default=0.0001,type=float, help='learning rate')
parser.add_argument('--writelib',default=1000,type=int, help='every this step to write parameter file')
parser.add_argument('--pr',default=10,type=int,help='every this step to print')
# parser.add_argument('--i',default=0,type=int,help='penalty for transition invariant')
parser.add_argument('--pi',default=1,type=int,help='regularize BO pi component')
parser.add_argument('--loss',default=1.0,type=float,help='the convergence criter of loss')
parser.add_argument('--convergence',default=0.999,type=float,help='the convergence criter of accuracy')
parser.add_argument('--maxepoch',default=100,type=int,help='the max training epoch')
parser.add_argument('--batch',default=100,type=int,help='the batch size of every configuration')
parser.add_argument('--t',default=0,type=int,help='optimize the three-boday term flag')
parser.add_argument('--h',default=0,type=int,help='optimize the hydrogen bond term flag')
parser.add_argument('--a',default=0,type=int,help='surpvise the angle term flag')
parser.add_argument('--f',default=0,type=int,help='optimize the four-boday term flag')
parser.add_argument('--bo',default=1,type=int,help='optimize the bond term flag')
parser.add_argument('--o',default=0,type=int,help='optimize the over energy term flag')
parser.add_argument('--zpe',default=1,type=int,help='re-calculate the zero point energy')
parser.add_argument('--vdw',default=1,type=int,help='optimize the vdw energy')
args = parser.parse_args(sys.argv[1:])

getdata = ColData()

dataset = {#'h22-v':'aimd_h22/h22-v.traj',
           #'nomb-c2h4c':'nomb_c2h4c.traj',
         #'cnt7-0':'data/cnt7-0.traj',
         #   'gpp-5':'data/gpp-5.traj',
         #   'gpp-4':'data/gpp-4.traj',
         #   'gpp-3':'data/gpp-3.traj',
         #   'gpp-2':'data/gpp-2.traj',
           # 'gpq-0':'gpq-0.traj',
           }
strucs = [#'gpdm',
         #  'h8',
         #  'h22',
         #  'c2h4',
         #  'c2h4c',
         #  'c2h6',
         #  'c3h8',
         #  # 'c3h5',
         #  'c4h10',
          'gpp',
          'gp',
          'gph',
          'gp4',
          'gp5',
          # 'gpd',
          #'gphit3',
          #'cnt32-1',
          #'cnt32',
          #'cnt4',
          #'cnt5',
          # 'cnt7',
          #'cnt8',
          # 'cnt10',
          #'c60',
          #'cnt333-2'
          ]
strucs = ['gp4','gp5','gp8',#'gpp'
          ]

data_invariant = []
weight={'c2h4':100.0,'gphit3':4.0,'cnt5-1':5.0,'nomb':0.0,'others':2.0}
batchs  = {'others':args.batch}
batch   = args.batch

for mol in strucs:
    b = batchs[mol] if mol in batchs else batchs['others']
    trajs = getdata(label=mol,batch=b)
    dataset.update(trajs)

clip = {'Desi':(100.0,725.0),
        'bo1':(-0.3,-0.002),'bo2':(4.0,9.6),'bo3':(-0.3,-0.003),'bo4':(4.0,9.6),
        'bo5':(-0.3,-0.003),'bo6':(4.0,9.6),
        'rosi':(0.5,1.46),'ropi':(0.5,1.46),'ropp':(0.5,1.46),
        'ovun1':(0.0,1.0),'ovun2':(-1.0,0.0),'ovun3':(0.0066,6.0),
        'rvdw_C':(1.851,2.4),
        'Devdw':(0.01,0.6),'alfa':(6.0,16.0),
        'vdw1':(0.54,8.0),
        'gammaw':(1.7,14.0),
        'val1':(10,60),'val2':(0.05,1.98),'val3':(0.01,7.6),
        'val4':(0.01,0.698), # cause NaN !!
        'tor1':(-2.3,-0.049),'tor2':(0.41,5.0),'tor3':(0.041,5.0),'tor4':(0.05,1.0),
        'V1':(0.0,20),'V2':(0,39),'V3':(0.0,10),
        'cutoff':(0.0001,0.01),'acut':(0.0001,0.05)}
 
cons = ['lp2',#'lp1',
        'theta0','valang','val8', #'val10',
        'valboc','cot1','cot2','coa1','coa2','coa3','coa4',
        'pen1','pen2','pen3','pen4',
        #'vdw1','gammaw','rvdw','alfa','Devdw',
        #'rohb','Dehb','hb1','hb2',
        'ovun6','ovun7','ovun8','ovun5',
        'val','lp3',#'cutoff',
        # 'acut',
        ]    # 不进行局域优化
 
if not args.bo:
   cons.extend(['Depi','Depp','Desi',
                'rosi','ropi','ropp',
                'bo1','bo2','bo3','bo4','bo5','bo6']) ### 
   mpopt = [0,0,0,0] # neural network for BO,MF,BE,VDW
elif args.bo==2:
   cons.extend(['rosi','ropi','ropp',
                'bo1','bo2','bo3','bo4','bo5','bo6']) ### 
   mpopt = [0,0,1,0]
elif args.bo==3:
   cons.extend([ 'rosi','ropi','ropp','bo1','bo2','bo3','bo4','bo5','bo6'])
   mpopt = [0,1,1,0]
   data_invariant = ['md-c2h4c-5.traj','md-c2h4c-2.traj',
                     'md-c2h4c-3.traj','md-c2h4c-4.traj','md-c2h4c-6.traj',
                     'md-c2h4c-7.traj','md-c2h4c-8.traj','md-c2h4c-9.traj']
elif args.bo==4:
   cons.extend(['Depi','Depp','Desi',
                'rosi','ropi','ropp',
                'bo1','bo2','bo3','bo4','bo5','bo6']) ### 
   mpopt = [0,1,0,0] # neural network for BO,MF,BE,VDW
   args.f=args.t=args.o=args.v=args.h=0
else:
   mpopt = [0,1,1,0]

if not args.f:
   cons.extend(['tor1','tor2','tor3','tor4','V1','V2','V3'])     # Four-body
              
if not args.t:
   cons.extend(['val1','val2','val3','val6','val7','val4','val5']) 
if not args.a:
   #cons += ['theta0','valang','val8']
   cons.extend(['theta0','vale','val9','val10','val8','valang']) # 

if not args.o:
   cons.extend(['ovun2','ovun3','ovun4','ovun1',
                'ovun6','ovun7','ovun8','ovun5'])
if not args.vdw:
   cons.extend(['vdw1','gammaw','rvdw','alfa','Devdw'])
if not args.h:
   cons.extend(['rohb','Dehb','hb1','hb2'])


pi_clip={'C-C-C':[(7.5,9.1,1.59,1.82)],
         #'C-C-H':[(7.5,8.5,1.15,1.8),(10.0,12,0.12,0.45)],
         #'H-C-H':[(7.5,8.5,1.15,1.8),(10.0,12,0.12,0.45)],
         }
be_clip = {'C-C':[(1.5,8.5,9.5,8.0,9.5,0.016,0.6)],
           'C-H':[(1.7,5.0,12,2.0,3,0.0,0.0)],
           'H-H':[(0.8,0,5,0,5,0.05,0.6)]}

bo_clip = {'C-C':[(1.6,6.8,7.5,6.8,7.5,0.0,2.0)]}
ang_clip= {'C-C-C':0.45}
rcut    = {"H-H":1.25,"C-C":1.6,"C-H":1.35,"others": 1.7}
# cons.extend(['Devdw','tor1'])
# cons.append('atomic')
# cons.append('V2')

if __name__ == '__main__':
   ''' train ''' 
   i    = 0 # iter
   loss = 0.0
   loss_= 0.0
   me   = 0.0
   me_  = 0.0
   rn = MPNN(libfile='ffield.json',
             dataset=dataset,   
             data_invariant=data_invariant,lambda_inv=1,rcut_inv=rcut,         
             weight=weight,
             optword='nocoul',
             # eaopt=['atomic'],#['Devdw','tor1','acut'],
             cons=cons,clip=clip,
             pi_clip=pi_clip,
             # ang_clip=ang_clip,
             # be_clip=be_clip,#
             # bo_clip=bo_clip,
             regularize_mf=1,regularize_be=1,regularize_bias=1,
             lambda_reg=0.001,lambda_bd=2.0,lambda_me=0.0002,lambda_pi=0.002,
             mf_layer=[9,2],be_layer=[9,1],
             EnergyFunction=1,MessageFunction=3,
             mf_universal_nn=None, #['C','H'],
             be_universal_nn=None, # ['C-H','H-H'],
             messages=1,
             mpopt=mpopt,
             bdopt=None,    # ['H-H'], 
             mfopt=None,    # ['H'], 
             batch_size=args.batch,
             fixrcbo=False,
             losFunc='n2',  # n2, mse, huber,abs
             convergence=0.97) 
   rn.initialize()
   rn.session(learning_rate=0.0001, method='AdamOptimizer') 
 
   p = {'atomic_C':0.0}
   for key in p:
       p[key] = rn.p_[key]
   m = 1.0
   # while True:
   #       for key in p:
   #           if m<0.0:
   #              p[key] -= 0.0001
   rn.update(p=p,reset_emol=True)      
   rn.get_zpe()
   rn.update(p=p,reset_emol=False) 
   rn.run(learning_rate=args.lr,
         step=args.step,
         print_step=args.pr,
         writelib=args.writelib,
         method='AdamOptimizer',
         close_session=False)
         # loss_= loss
         # me_  = me
         # loss = rn.loss_
         # me   = rn.ME_
         # if i>0:
         #    los = loss - loss_
         #    m   = me - me_
         #    if los>0.0001:
         #       break
         # i += 1

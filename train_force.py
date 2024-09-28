#!/usr/bin/env python
from os import  system
import torch
from ase import Atoms
from irff.reax_force import ReaxFF_nn_force
from irff.data.ColData import ColData

getdata = ColData()

dataset = { # 'gp4':'data/gp4-0.traj','gp5':'data/gp5-0.traj'
    }
strucs = ['gp4','gp5',#'gpp'
          ]

batchs  = {'others':50}

for mol in strucs:
    b = batchs[mol] if mol in batchs else batchs['others']
    trajs = getdata(label=mol,batch=b)
    dataset.update(trajs)

while True:
    rn = ReaxFF_nn_force(dataset={'gp4':'data/gp4-0.traj'},
                        weight_energy={'others':1.0},
                        weight_force={'others':1.0},
                        cons=['acut'],
                        libfile='ffield.json',
                        screen=True,
                        # batch=50,
                        device='cuda')
    # rn.cuda()
    # rn.compile()
    optimizer = torch.optim.Adam(rn.parameters(), lr=0.0001 )
    iter_num = 5001
    for step in range(iter_num):
        E,F = rn()
        loss = rn.get_loss()
        optimizer.zero_grad()

        if step%10==0:
           print( "{:8d} loss: {:10.5f}   energy: {:10.5f}   force: {:10.5f}".format(step,
                    loss.item(),rn.loss_e.item(),rn.loss_f.item()))
           if step%1000==0:
              # rn.save_ffield('ffield_{:d}.json'.format(step))
              rn.save_ffield('ffield.json')
              # system('cp ffield.json ffield_{:d}.json'.format(step))
        loss.backward(retain_graph=True)
        optimizer.step()

    # rn.save_ffield('ffield.json')

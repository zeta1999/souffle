# -*- coding: utf-8 -*-
"""
Created on Sun Nov 12 18:09:46 2017

@author: andres
"""

import SwigInterface
p=SwigInterface.newInstance('flights')
p.loadAll('.')
p.run()
p.printAll('.')
p.thisown=1
del p
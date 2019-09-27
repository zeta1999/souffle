# -*- coding: utf-8 -*-
"""
Created on Sun Nov 12 18:09:46 2017

@author: andres
"""

import SwigInterface
p=SwigInterface.newInstance('insert_for')
p.loadAll('facts/.')
p.run()
p.printAll('.')
p.thisown=1
del p

#mydict={ 'a' : 'b', 'c' : 'd' ,'d':'a'}
#insta="insert_for"
#rela="edge"
#rela_out="path"
#SwigInterface.loadInput(mydict,insta,rela,rela_out)

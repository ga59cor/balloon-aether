#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Thu May 24 01:39:31 2018

@author: pi
"""

import fileinput

with fileinput.FileInput(filename, inplace=True, backup='.bak') as file:
    for line in file:
        print(line.replace(text_to_search, replacement_text), end='')
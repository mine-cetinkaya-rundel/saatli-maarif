#!/usr/bin/python3
# -*- coding:utf-8 -*-
import sys
import os

from lib import epd7in5b_HD
import time
from PIL import Image,ImageDraw,ImageFont
import traceback


try:
    font24 = ImageFont.truetype(os.path.join('pic', 'Font.ttc'), 24)
    font18 = ImageFont.truetype(os.path.join('pic', 'Font.ttc'), 18)

    epd = epd7in5b_HD.EPD()

    epd.init()
    epd.Clear()

    epd.sleep()
    
    epd.Dev_exit()
        
except KeyboardInterrupt:    
    epd7in5b_HD.epdconfig.module_exit()
    exit()

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

    print("initialize ...")
    epd.init()
    print("done")

    print("clear ...")
    epd.Clear()
    print("done")

    black_image = Image.new('1', (epd.width, epd.height), 255)  # 255: clear the frame
    draw_black = ImageDraw.Draw(black_image)
    
    for i in range(10):
        draw_black.text(
            (24*i, 24*i), 
            'Hello World!', font = font24, fill = 0
        )

    print("display 1 ...")
    epd.display_black(epd.getbuffer(black_image))
    print("done")

    time.sleep(5)
    
    black_image = Image.new('1', (epd.width, epd.height), 255)  # 255: clear the frame
    draw_black = ImageDraw.Draw(black_image)
    
    
    for i in range(10):
        draw_black.ellipse(
            (epd.width/2 - i * 10, epd.height/2 - i * 10, 
             epd.width/2 + i * 10, epd.height/2 + i * 10), 
            outline = 0
        )
    
    print("display 2 ...")
    epd.display_black(epd.getbuffer(black_image))
    print("done")

    epd.sleep()
    
    epd.Dev_exit()
        
except KeyboardInterrupt:    
    epd7in5b_HD.epdconfig.module_exit()
    exit()

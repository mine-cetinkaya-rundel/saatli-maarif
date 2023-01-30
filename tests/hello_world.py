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

    black_image = Image.new('1', (epd.width, epd.height), 255)  # 255: clear the frame
    draw_black = ImageDraw.Draw(black_image)
    
    for i in range(10):
        draw_black.text(
            (24*i, 24*i), 
            'Hello World!', font = font24, fill = 0
        )


    red_image   = Image.new('1', (epd.width, epd.height), 255)  # 255: clear the frame
    draw_red = ImageDraw.Draw(red_image)
    
    for i in range(10):
        draw_red.text(
            (24*i, epd.height - 24*(i+1)), 
            'Hello Mellow!', font = font24, fill = 0
        )

    
    for i in range(10):
        draw_black.ellipse(
            (epd.width/2 - i * 10, epd.height/2 - i * 10, 
             epd.width/2 + i * 10, epd.height/2 + i * 10), 
            outline = 0
        )
        
        draw_red.ellipse(
            (epd.width/2 - i * 10 - 5, epd.height/2 - i * 10 - 5, 
             epd.width/2 + i * 10 + 5, epd.height/2 + i * 10 + 5), 
            outline = 0
        )
    
    epd.display(epd.getbuffer(black_image), epd.getbuffer(red_image))
    

    epd.sleep()
    
    epd.Dev_exit()
        
except KeyboardInterrupt:    
    epd7in5b_HD.epdconfig.module_exit()
    exit()

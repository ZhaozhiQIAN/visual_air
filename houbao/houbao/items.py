# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class HoubaoItem(scrapy.Item):
    # define the fields for your item here like:
    # name = scrapy.Field()
    province = scrapy.Field()
    city = scrapy.Field()
    date = scrapy.Field()
    pm25 = scrapy.Field()
    pm10 = scrapy.Field()
    co = scrapy.Field()
    no2 = scrapy.Field()
    so2 = scrapy.Field()
    pass

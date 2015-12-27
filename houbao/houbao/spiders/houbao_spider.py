import scrapy
from houbao.items import HoubaoItem


class houbao_spider(scrapy.Spider):
    name = 'Houbao'
    start_urls = ['http://www.tianqihoubao.com/aqi/']

    def viso(self, a):
        b = ''
        for item in a:
            b += chr(ord(item))
        d = b.decode('gbk').encode('utf-8')
        return d

    # first level parsing: get city urls
    def parse(self, response):
        citychunck = response.css('.citychk')
        for prorow in citychunck.css('dl')[1:]:
            province = prorow.css('dt > b::text').extract()[0]
            for citycell in prorow.css('dd > a'):
                cityname = citycell.css('a::text').extract()[0]
                cityurl = 'http://www.tianqihoubao.com' + citycell.css('a::attr(href)').extract()[0]
                request = scrapy.Request(cityurl, callback=self.parse_town)
                request.meta['province'] = self.viso(province).strip()
                request.meta['cityname'] = self.viso(cityname).strip()
                yield request

    # second level parsing: parse each town
    def parse_town(self, response):
        monchunck = response.css('div.box:nth-child(3) > ul:nth-child(2)')
        for moncell in monchunck.css('li'):
            monthurl = 'http://www.tianqihoubao.com' + moncell.css('::attr(href)').extract()[0]
            request = scrapy.Request(monthurl, callback=self.parse_date)
            request.meta['province'] = response.meta['province']
            request.meta['cityname'] = response.meta['cityname']
            yield request

    # third level parsing: parse each date
    def parse_date(self, response):
        tbl = response.css('.b') # .b > tbody:nth-child(1)
        for row in tbl.css('tr')[1:]:
            item = HoubaoItem()
            item['date'] = row.css('td::text')[0].extract().lstrip()
            item['pm25'] = row.css('td::text')[-5].extract().lstrip()
            item['pm10'] = row.css('td::text')[-4].extract().lstrip()
            item['co'] = row.css('td::text')[-3].extract().lstrip()
            item['no2'] = row.css('td::text')[-2].extract().lstrip()
            item['so2'] = row.css('td::text')[-1].extract().lstrip()
            item['city'] = response.meta['cityname']
            item['province'] = response.meta['province']
            yield item





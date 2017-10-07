---
title: "Notes to Self: The Python csv Library"
date: 2017-09-14	
tags: notestoself,python,csv
layout: post
cover: red.png
published: True
---

*This is the beginning of a series called "Notes to Self." It'll be a bunch of articles about things I wish I knew when I started programming. Enjoy!*

When I started in tech about eight months ago, it was as an intern at the research institute where I now work as a Junior Developer. In both of these positions I've been subject to my fair share of grunt work, up to and including massaging old data to make it presentable in reports. I do this type of thing fairly often, and I want to share my secret weapon: the [Python csv library](https://docs.python.org/3.6/library/csv.html), specifically the [DictReader](https://docs.python.org/3.6/library/csv.html#csv.DictReader) class.

What the DictReader class allows you to do is read lines from a csv (I typically use [Sequel Pro](https://www.sequelpro.com/) to export SQL views as csv files) as dictionaries, which allows you to easily access values by their keys. You can then manipulate the values and then write them to a new csv, provided that you know a little bit about [Python File I/O](https://www.tutorialspoint.com/python/python_files_io.htm). It's a pretty intuitive process, and has made my work easier countless times. 

I made a little [repo](https://github.com/nataliejedson/presidential-race) with some demo code exhibiting the power of this library. It takes some data about a hypothetical race between the first ten Presidents of the United States and makes it more readable. 

Hope this helps you as much as it has helped me!
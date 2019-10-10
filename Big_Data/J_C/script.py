#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import time, asyncio

async def say(n,what):
  for i in range(1,n):
    time.sleep(0.1)
    print(what)


loop = asyncio.get_event_loop() 
loop.run_until_complete(
    asyncio.gather(
        say(3,'hello'),
        say(5,'world !')
    )
)
loop.close()
import os, time, random, base64
import numpy as np
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import cv2

options = webdriver.ChromeOptions()
# options.setBinary("/usr/bin/brave-browser")
options.binary_location = "/usr/bin/brave-browser"
driver_path = os.path.join(os.getcwd(), "chromedriver")
driver = webdriver.Chrome(options=options, executable_path="./chromedriver_linux64/chromedriver")

# driver.get("https://play2048.co")
# driver.get("https://play2048game.net/game/2048-3x3/#:~:text=2048%203x3%20game%20is%20web,right%20on%20the%20game%20board.")
try:
    driver.get("brave://dino")
except:
    pass

time.sleep(10)
htmlElem = driver.find_element_by_tag_name('body')
# htmlElem.send_keys(Keys.DOWN)
# time.sleep(3)
# htmlElem.send_keys(Keys.DOWN)

# Start game
htmlElem.send_keys(Keys.SPACE)

while True:
    canvas = driver.execute_script("return document.querySelector('canvas.runner-canvas').toDataURL()")
    termination = driver.execute_script("return Runner.instance_.playing")
    if not termination:
        print("Game over!")
        canvas = canvas[22:]
        _nparr = np.frombuffer(base64.b64decode(canvas), np.uint8)
        _img = cv2.imdecode(_nparr, cv2.IMREAD_COLOR)
        cv2.imshow("Game Over", _img)
        cv2.waitKey(0)
        break

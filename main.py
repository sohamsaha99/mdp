import os, time, random
from selenium import webdriver
from selenium.webdriver.common.keys import Keys

options = webdriver.ChromeOptions()
# options.setBinary("/usr/bin/brave-browser")
options.binary_location = "/usr/bin/brave-browser"
driver_path = os.path.join(os.getcwd(), "chromedriver")
driver = webdriver.Chrome(options=options, executable_path="./chromedriver_linux64/chromedriver")

# driver.get("https://play2048.co")
# driver.get("https://play2048game.net/game/2048-3x3/#:~:text=2048%203x3%20game%20is%20web,right%20on%20the%20game%20board.")
driver.get("https://cyberzhg.github.io/2048/index.html?size=2&mode=normal")
# try:
#     driver.get("brave://dino")
# except:
#     pass

time.sleep(10)
htmlElem = driver.find_element_by_tag_name('html')
# htmlElem.send_keys(Keys.DOWN)
# time.sleep(3)
# htmlElem.send_keys(Keys.DOWN)

while True:
    r = random.randint(0, 4)
    if r==0:
        htmlElem.send_keys(Keys.DOWN)
    elif r==1:
        htmlElem.send_keys(Keys.LEFT)
    elif r==2:
        htmlElem.send_keys(Keys.UP)
    else:
        htmlElem.send_keys(Keys.RIGHT)

    time.sleep(0.2)
    # print(r)

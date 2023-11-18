import requests
from bs4 import BeautifulSoup
import re
import os
import json

# def getMajorLinks(url):
#     majorLinks = {}
#     if os.path.isfile("majors.json"):
#         with open("majors.json", "r") as f:
#             majorLinks = json.load(f)
#     else:
#         response = requests.get(url)
#         html = BeautifulSoup(response.text, 'html.parser')
#         majorList = html.find_all('ul')[20]
#         ulParser = BeautifulSoup(str(majorList), 'html.parser')
#         liElements = ulParser.find_all('li')
#         for li in liElements:
#             a = li.find('a')
#             majorLinks[a.text] = f"https://catalog.usu.edu/{a.get('href')}"
#         with open("majors.json", "w") as f:
#             json.dump(majorLinks, f, indent=4)
#     return majorLinks


# def savePreReqs(majors):
#     for k,v in majors.items():
#         with open("courses", 'w') as f:
#             response = requests.get(v)
#             html = BeautifulSoup(response.text, 'html.parser')
#             courseList = html.find_all('div', 'acalog-core')
#             print(courseList)

# majors = getMajorLinks("https://catalog.usu.edu/content.php?catoid=38&navoid=29050")

NUM_PAGES = 73

def parseCourses(url):
    courses = []
    response = requests.get(url)
    html = BeautifulSoup(response.text, "html.parser").find('td', 'block_content').find_all('table')[2].find_all('tr')
    for row in html:
        a = row.find('a')
        if a:
            entry = findPreReq(a.get('href'))
            if entry:
                courses.append(entry)
    return courses


def findPreReq(url):
    response = requests.get("https://catalog.usu.edu/" + url)
    html = BeautifulSoup(response.text, "html.parser")
    currCourse = html.find('h1').text.split()
    try:
        if int(currCourse[1][0]) <= 5:
            course = {}
            course[currCourse[0].lower() + "-" + currCourse[1]] = []
            preReqs = html.find('td', 'block_content').find_all('a', attrs={"rel": "remote"})
            for req in preReqs:
                req = req.text.replace(" ", "-").lower().strip()
                if (re.match(r'^[a-z]{2,4}-\d{4}$', req)):
                    course[currCourse[0].lower() + "-" + currCourse[1]].append(req)
                else:
                    print("Invalid course id: ", req)
            return course
    except Exception as e:
        print(e)



allCourses = {}
allCourses['data'] = []
for i in range(NUM_PAGES):
    print(i)
    currCourse = parseCourses(f"https://catalog.usu.edu/content.php?catoid=38&catoid=38&navoid=28875&filter%5Bitem_type%5D=3&filter%5Bonly_active%5D=1&filter%5B3%5D=1&filter%5Bcpage%5D={i + 1}#acalog_template_course_filter")
    allCourses['data'] += currCourse

with open("allData.json", 'w') as f:
    json.dump(allCourses, f, indent=4)
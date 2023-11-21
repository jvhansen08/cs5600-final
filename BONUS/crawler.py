import requests
from bs4 import BeautifulSoup
import re
import json


NUM_PAGES = 73

def parseCourses(url, allCourses):
    response = requests.get(url)
    html = BeautifulSoup(response.text, "html.parser").find('td', 'block_content').find_all('table')[2].find_all('tr')
    for row in html:
        a = row.find('a')
        if a:
            findPreReq(a.get('href'), allCourses)

def findPreReq(url, allCourses):
    response = requests.get("https://catalog.usu.edu/" + url)
    html = BeautifulSoup(response.text, "html.parser")
    currCourseRaw = html.find('h1').text.split()
    currCourse = currCourseRaw[0].lower() + '-' + currCourseRaw[1]
    try:
        if int(currCourseRaw[1][0]) <= 5:
            allCourses[currCourse] = []
            preReqs = html.find('td', 'block_content').find_all('a', attrs={"rel": "remote"})
            for req in reversed(preReqs):
                req = req.text.replace(" ", "-").lower().strip()
                if re.match(r'^[a-z]{2,4}-\d{3,4}$', req) and not int(req.split("-")[1][0]) > 5 and not (req in allCourses and currCourse in allCourses[req]):
                    allCourses[currCourse].append(req)
                else:
                    print("Failed to add course: ", req)
            return allCourses
    except Exception as e:
        print(e)

def main():
    allCourses = {}
    for i in range(NUM_PAGES):
        parseCourses(f"https://catalog.usu.edu/content.php?catoid=38&catoid=38&navoid=28875&filter%5Bitem_type%5D=3&filter%5Bonly_active%5D=1&filter%5B3%5D=1&filter%5Bcpage%5D={i + 1}#acalog_template_course_filter", allCourses)

    with open("allData.json", 'w') as f:
        json.dump(allCourses, f, indent=4)

main()
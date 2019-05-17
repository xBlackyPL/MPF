import sys
import csv
import math
from PIL import Image
from PIL import ImageDraw
from PIL import ImageFont


def main():
    input_file = sys.argv[1]
    rows = []
    dates = []
    font = ImageFont.truetype("arial.ttf", 16)
    with open(input_file) as csv_file:
        reader = csv.reader(csv_file, delimiter=',')
        row_iterator = iter(reader)
        next(row_iterator)
        row_index = 0
        for row in row_iterator:
            rows.append([])
            element_iterator = iter(row)
            dates.append(next(element_iterator))
            for elem in element_iterator:
                rows[row_index].append(90 - (float(elem) * -1))
            row_index += 1

    print(len(dates))
    frames = []
    row_index = 0

    for row in rows:
        x = []
        y = []
        print(str(row_index))
        elem_index = 0
        for elem in row:
            x.append((elem * 2 * math.pi * 215) / 360 * math.cos(elem_index * (math.pi / 180)) + 256)
            y.append((elem * 2 * math.pi * 215) / 360 * math.sin(elem_index * (math.pi / 180)) + 256)
            elem_index += 1

        points = []
        for index in range(elem_index):
            points.append((x[index], y[index]))

        image = Image.new("RGB", (512, 512))
        draw = ImageDraw.Draw(image)
        draw.polygon(points, fill=(255, 255, 255))
        draw.text((0, 0), "[Antarctica Ice Sheet]", (255, 255, 255), font=font)
        draw.text((0, 18), "Observation Date: " + dates[row_index], (255, 255, 255), font=font)
        frames.append(image)
        row_index += 1

    frames[0].save('daily_ice_edge.gif', format='GIF', append_images=frames[1:], save_all=True, duration=50, loop=0)


if __name__ == '__main__':
    main()

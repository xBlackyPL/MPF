#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "CImg/CImg.h"
#include "entities.hpp"

void laplace(float **vec, int xAxisBound, int yAxisBound, Entity *heater,
             Entity *searcher) {
    for (int i = 1; i < xAxisBound - 1; ++i) {
        for (int j = 1; j < yAxisBound - 1; ++j) {
            if ((heater->x != i && heater->y != j) ||
                (searcher->x != i && searcher->y != j)) {
                vec[i][j] = 0.25f * (vec[i - 1][j] + vec[i + 1][j] +
                                     vec[i][j + 1] + vec[i][j - 1]);
            }
        }
    }
}

int updateBoard(float **vec, int xAxisBound, int yAxisBound, Entity *heater,
                Entity **searcher) {
    randomMoveEntity(heater, xAxisBound, yAxisBound);
    vec[heater->x][heater->y] = 100;
    int result =
        searchForTheForce(vec, searcher, xAxisBound, yAxisBound, 100, 2.0f);
    vec[(*searcher)->x][(*searcher)->y] = 100;
    return result;
}

int *normalizeData(float **vec, int xAxisBound, int yAxisBound) {
    int *array = (int *)calloc(xAxisBound * yAxisBound, sizeof(int));
    for (int i = 0; i < xAxisBound; ++i) {
        for (int j = 0; j < yAxisBound; ++j) {
            array[i * xAxisBound + j] = (int)(vec[i][j] * 10);
        }
    }
    return array;
}

void saveOutputAsImage(float **vec, int xAxisBound, int yAxisBound, int index) {
    int *normalizedImage = normalizeData(vec, xAxisBound, yAxisBound);
    cimg_library::CImg<int> img(normalizedImage, xAxisBound, yAxisBound);

    char path[32];
    char *fileName = "Output/Image_";
    snprintf(path, 32, "%s%d", fileName, index);

    img.save_png(path, 8);

    free(normalizedImage);
}

void runSimulation(float **vec, float **tmpVec, int xAxisBound, int yAxisBound,
                   int samplingFrequency, Entity *heater, Entity **searcher) {
    int index = 0;

    int flag = 1;
    laplace(vec, xAxisBound, yAxisBound, heater, *searcher);
    updateBoard(vec, xAxisBound, yAxisBound, heater, searcher);

    while (flag) {
        for (int i = 0; i < xAxisBound; ++i) {
            for (int j = 0; j < yAxisBound; ++j) {
                tmpVec[j][i] = vec[j][i];
            }
        }
        laplace(vec, xAxisBound, yAxisBound, heater, *searcher);

        if (index % samplingFrequency == 0) {
            saveOutputAsImage(vec, xAxisBound, yAxisBound, index);
            printf("%d\n", index);
            flag = updateBoard(vec, xAxisBound, yAxisBound, heater, searcher);
        }
        index++;
    }
}

int main() {
    int xAxisBound = 100;
    int yAxisBound = 100;

    srand(time(NULL));

    float **x = (float **)calloc(xAxisBound, sizeof(float *));
    float **xtemp = (float **)calloc(xAxisBound, sizeof(float *));

    for (int i = 0; i < xAxisBound; ++i) {
        x[i] = (float *)calloc(yAxisBound, sizeof(float));
        xtemp[i] = (float *)calloc(yAxisBound, sizeof(float));
    }

    Entity *heater = spawnEntityOnRandomPosition(xAxisBound, yAxisBound);
    Entity *searcher = spawnEntityOnRandomPosition(xAxisBound, yAxisBound);

    runSimulation(x, xtemp, xAxisBound, yAxisBound, 10, heater, &searcher);

    return 0;
}

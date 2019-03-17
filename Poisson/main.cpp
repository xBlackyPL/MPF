#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "CImg/CImg.h"
#include "entities.hpp"
#include "generateImg.hpp"

void laplace(float **vec, int xAxisBound, int yAxisBound, Entity *heater) {
    for (int i = 1; i < xAxisBound - 1; ++i) {
        for (int j = 1; j < yAxisBound - 1; ++j) {
            if (heater->x != i && heater->y != j) {
                vec[i][j] = 0.25f * (vec[i - 1][j] + vec[i + 1][j] +
                                     vec[i][j + 1] + vec[i][j - 1]);
            }
        }
    }
}

int updateBoard(float **vec, int **path, int xAxisBound, int yAxisBound,
                Entity *heater, Entity **searcher) {
    randomMoveEntity(heater, xAxisBound, yAxisBound);
    vec[heater->x][heater->y] = 100.f;
    // int result =
    //     searchForTheForce(vec, searcher, heater, xAxisBound, yAxisBound);

    int result =
        naiveForTheForce(vec, searcher, heater, xAxisBound, yAxisBound);
    path[(*searcher)->x][(*searcher)->y] = 1;
    return result;
}

void printOutputFile(float **vec, int **path, int xAxisBound, int yAxisBound) {
    FILE *output = fopen("Output.txt", "a+");
    FILE *output_path = fopen("Output_Path.txt", "a+");

    for (int i = 0; i < xAxisBound; ++i) {
        for (int j = 0; j < yAxisBound; ++j) {
            fprintf(output, "%.2lf ", vec[i][j]);
            fprintf(output_path, "%d ", path[i][j]);
        }
        fprintf(output, "\n");
        fprintf(output_path, "\n");
    }
    fprintf(output, "\n");
    fprintf(output_path, "\n");

    fclose(output);
    fclose(output_path);
}

void runSimulation(float **vec, float **tmpVec, int **path, int xAxisBound,
                   int yAxisBound, int samplingFrequency, Entity *heater,
                   Entity **searcher) {
    int index = 0;
    int flag = 1;

    laplace(vec, xAxisBound, yAxisBound, heater);
    updateBoard(vec, path, xAxisBound, yAxisBound, heater, searcher);

    while (flag) {
        for (int i = 0; i < xAxisBound; ++i) {
            for (int j = 0; j < yAxisBound; ++j) {
                tmpVec[j][i] = vec[j][i];
            }
        }
        laplace(vec, xAxisBound, yAxisBound, heater);

        if (index % samplingFrequency == 0) {
            printOutputFile(vec, path, xAxisBound, yAxisBound);
            flag = updateBoard(vec, path, xAxisBound, yAxisBound, heater,
                               searcher);
        }
        index++;
        if (index == 1000000) break;
    }
}

int main() {
    int xAxisBound = 100;
    int yAxisBound = 100;

    srand(time(NULL));

    float **boards[2] = {(float **)calloc(xAxisBound, sizeof(float *)),
                         (float **)calloc(xAxisBound, sizeof(float *))};

    int **path = (int **)calloc(xAxisBound, sizeof(int *));

    for (int i = 0; i < 2; ++i) {
        for (int j = 0; j < xAxisBound; ++j) {
            boards[i][j] = (float *)calloc(yAxisBound, sizeof(float));
            boards[i][j] = (float *)calloc(yAxisBound, sizeof(float));
            path[j] = (int *)calloc(yAxisBound, sizeof(int));
        }
    }

    Entity *heater = spawnEntityOnRandomPosition(xAxisBound, yAxisBound);
    Entity *searcher = spawnEntityOnRandomPosition(xAxisBound, yAxisBound);

    runSimulation(boards[0], boards[1], path, xAxisBound, yAxisBound, 10,
                  heater, &searcher);

    for (int i = 0; i < 2; ++i) {
        for (int j = 0; j < xAxisBound; ++j) {
            free(boards[i][j]);
        }
        free(boards[i]);
    }

    for (int i = 0; i < xAxisBound; ++i) {
        free(path[i]);
    }

    free(path);

    return 0;
}

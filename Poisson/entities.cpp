#include "entities.hpp"
#include <stdlib.h>

void randomMoveEntity(Entity* entity, int xSize, int ySize) {
    int entityXAxisNew = entity->x + rand() % 3 - 1;
    int entityYAxisNew = entity->y + rand() % 3 - 1;

    entity->x = entityXAxisNew > xSize - 2 || entityXAxisNew < 1
                    ? entity->x
                    : entityXAxisNew;

    entity->y = entityYAxisNew > ySize - 2 || entityYAxisNew < 1
                    ? entity->y
                    : entityYAxisNew;
}

Entity* spawnEntityOnRandomPosition(int xAxisBound, int yAxisBound) {
    Entity* newEntity = (Entity*)calloc(1, sizeof(Entity));
    newEntity->x = rand() % (xAxisBound - 2) + 1;
    newEntity->y = rand() % (yAxisBound - 2) + 1;
    return newEntity;
}

int searchForTheForce(float** vec, Entity** entity, int xAxisBound,
                      int yAxisBound, int force, float eps) {
    if (abs(force - vec[(*entity)->x][(*entity)->y]) < eps) {
        return 0;
    }
    float upper = 0.0f;
    float lower = 0.0f;
    float right = 0.0f;
    float left = 0.0f;

    upper = (*entity)->y - 1 > 1 ? vec[(*entity)->x][(*entity)->y - 1] : upper;
    lower = (*entity)->y + 1 < yAxisBound ? vec[(*entity)->x][(*entity)->y + 1]
                                          : lower;
    right = (*entity)->x + 1 < xAxisBound ? vec[(*entity)->x + 1][(*entity)->y]
                                          : right;
    left = (*entity)->x - 1 > 1 ? vec[(*entity)->x - 1][(*entity)->y] : left;

    if (upper > lower) {
        if (upper > vec[(*entity)->x][(*entity)->y]) {
            (*entity)->y++;
        }
    } else {
        if (lower > vec[(*entity)->x][(*entity)->y]) {
            (*entity)->y--;
        }
    }

    if (right > left) {
        if (right > vec[(*entity)->x][(*entity)->y]) {
            (*entity)->x++;
        }
    } else {
        if (left > vec[(*entity)->x][(*entity)->y]) {
            (*entity)->x--;
        }
    }

    if (upper < eps && lower < eps && right < eps && left < eps) {
        free(*entity);
        *entity = spawnEntityOnRandomPosition(xAxisBound, yAxisBound);
    }

    return 1;
}
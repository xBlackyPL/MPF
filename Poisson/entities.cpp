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

float getFieldValue(float** vec, int xAxisBound, int yAxisBound, int x, int y) {
    float result = 0.f;
    if (x >= 1 && x <= xAxisBound - 2 && y >= 1 && y <= yAxisBound - 2) {
        result = vec[x][y];
    }

    return result;
}

int searchForTheForce(float** vec, Entity** entity, Entity* heater,
                      int xAxisBound, int yAxisBound) {                          
    if ((*entity)->x == heater->x && (*entity)->y == heater->y) {
        return 0;
    }

    float currentValue = 0.f;
    float bestOptionValue = 0.f;
    Entity transitionVector = {0, 0};

    for (int i = -1; i < 2; ++i) {
        for (int j = -1; j < 2; ++j) {
            currentValue = getFieldValue(vec, xAxisBound, yAxisBound,
                                         (*entity)->x + i, (*entity)->y + j);

            if (currentValue > bestOptionValue) {
                bestOptionValue = currentValue;
                transitionVector.x = i;
                transitionVector.y = j;
            }
        }
    }

    (*entity)->x += transitionVector.x;
    (*entity)->y += transitionVector.y;

    return 1;
}
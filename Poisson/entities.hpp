typedef struct EntityPoint {
    int x;
    int y;
} Entity;

int searchForTheForce(float** vec, Entity** entity, Entity* heater,
                      int xAxisBound, int yAxisBound);

int naiveForTheForce(float** vec, Entity** entity, Entity* heater,
                     int xAxisBound, int yAxisBound);

float getFieldValue(float** vec, int xAxisBound, int yAxisBound, int x, int y);

void randomMoveEntity(Entity* entity, int xAxisBound, int yAxisBound);

Entity* spawnEntityOnRandomPosition(int xAxisBound, int yAxisBound);
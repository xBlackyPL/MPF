typedef struct EntityPoint {
  int x;
  int y;
} Entity;

int searchForTheForce(float** vec,
                      Entity** entity,
                      int xAxisBound,
                      int yAxisBound,
                      int force,
                      float eps);

void randomMoveEntity(Entity* entity, int xAxisBound, int yAxisBound);
Entity* spawnEntityOnRandomPosition(int xAxisBound, int yAxisBound);
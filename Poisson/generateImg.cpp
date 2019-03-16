#include "generateImg.hpp"

using namespace ImageSaver;

int *normalizeData(float **vec, int xAxisBound, int yAxisBound) {
    int *array = (int *)calloc(xAxisBound * yAxisBound, sizeof(int));
    for (int i = 0; i < xAxisBound; ++i) {
        for (int j = 0; j < yAxisBound; ++j) {
            array[i * xAxisBound + j] = (int)(vec[i][j]);
        }
    }
    return array;
}

void saveOutputAs8bitImage(int *vec, uint32_t width, uint32_t height,
                           char *pathBase, int id) {
    cimg_library::CImg<int> img(vec, width, height);
    std::string path = pathBase;
    path += "_" + id;

    img.save_png(path.c_str(), 8);
}

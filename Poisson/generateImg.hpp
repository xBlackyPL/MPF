#include <cstdint>
#include <memory>
#include <string>

#include "CImg/CImg.h"

/*
    build flags -O2 -L/usr/X11R6/lib -lm -lpthread -lX11
*/

namespace ImageSaver {
const std::string defaultOutputFileName = "OutputImg";
int *normalizeData(float **vec, uint32_t xAxisLength, uint32_t yAxisLength);
void saveOutputAs8bitImage(int *vec, uint32_t width, uint32_t height,
                           const char *path = defaultOutputFileName.c_str(),
                           int id = 0);
}  // namespace ImageSaver
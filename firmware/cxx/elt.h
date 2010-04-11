#ifndef ELT_H
#define ELT_H

#include <stdint.h>

#ifdef __cplusplus
#endif

class Element8 {
private:
  uint8_t data;
public:
  Element8& operator++() {
    ++data;
  };
};

class Element16 {
private:
  uint16_t data;
public:
  Element16& operator++() {
    ++data;
  };
};

class Element24 {
private:
  uint8_t data[3];
public:
  Element24& operator++() {
    ++data[0];
    ++data[1];
    ++data[2];
  };
};

class Element32 {
private:
  uint32_t data;
public:
  Element32& operator++() {
    ++data;
  };
};

#ifdef __cplusplus
#endif

#endif

#ifndef HISTOGRAM_H
#define HISTOGRAM_H

extern volatile uint32_t histogram[];
extern volatile uint8_t  total_counter8;
extern volatile uint16_t total_counter16;
extern volatile uint32_t total_counter32;
extern volatile uint64_t total_counter64;

void histogram_init(void);
void histogram_update(const uint16_t value);

#endif /* !HISTOGRAM_H */

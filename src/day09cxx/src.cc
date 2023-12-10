#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <cstdint>
#include <cstdio>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fstream>
#include <iostream>
#include <numeric>
#include <sstream>
#include <vector>

int_fast32_t extrapolate(std::istringstream& stream) {
  int_fast32_t sequence[21] = {0};
  std::size_t index = 0;

  int_fast32_t number;
  while (stream >> number) {
    sequence[index++] = number;
    for (auto i = sequence + index - 2; i >= sequence; --i) {
      *i = *(i + 1) - *i;
    }
    if (stream.peek() == '\n') {
      stream.get();
      break;
    }
  }

  return std::accumulate(sequence, sequence + index, 0LL);
}

int main() {
  auto filename = std::getenv("INPUT");
  if (filename == nullptr) {
    return EXIT_FAILURE;
  }

  auto fd = open(filename, O_RDONLY);
  if (fd == -1) {
    return EXIT_FAILURE;
  }

  struct stat st{};
  if (fstat(fd, &st) == -1) {
    close(fd);
    return EXIT_FAILURE;
  }

  auto size = st.st_size;
  char* addr = (char *)mmap(nullptr, size, PROT_READ, MAP_PRIVATE, fd, 0);
  madvise(addr, size, MADV_SEQUENTIAL);
  madvise(addr, size, MADV_WILLNEED);
  madvise(addr, size, MADV_HUGEPAGE);
  if (addr == MAP_FAILED) {
    close(fd);
    return EXIT_FAILURE;
  }

  auto input = std::istringstream(addr);
  auto start = std::chrono::high_resolution_clock::now();
  int_fast32_t total = 0;
  while (input.peek() != '\n' && input.peek() != EOF) {
    total += extrapolate(std::ref(input));
  }
  auto end = std::chrono::high_resolution_clock::now();

  auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start);
  std::cout << duration << std::endl;
  std::cout << total << std::endl;
  munmap(addr, size);
  close(fd);

  return EXIT_SUCCESS;
}

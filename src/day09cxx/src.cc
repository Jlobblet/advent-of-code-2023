#include <algorithm>
#include <chrono>
#include <cstdlib>
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

long long extrapolate(std::string line) {
  static std::optional<std::size_t> length = std::nullopt;

  std::istringstream line_stream(line);
  std::vector<long long> sequence;

  if (length.has_value()) {
    sequence.reserve(length.value());
  }

  long long number;
  while (line_stream >> number) {
    sequence.push_back(number);
    for (auto i = sequence.end() - 2; i >= sequence.begin(); --i) {
      *i = *(i + 1) - *i;
    }
  }

  if (!length.has_value()) {
    length = sequence.size();
  }

  return std::accumulate(sequence.begin(), sequence.end(), 0LL);
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
  if (addr == MAP_FAILED) {
    close(fd);
    return EXIT_FAILURE;
  }

  auto input = std::istringstream(std::string(addr, size));
  std::string line;
  auto start = std::chrono::high_resolution_clock::now();
  long long total = 0;
  while (std::getline(input, line)) {
    total += extrapolate(line);
  }
  auto end = std::chrono::high_resolution_clock::now();

  auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start);
  std::cout << duration << std::endl;
  std::cout << total << std::endl;
  close(fd);

  return EXIT_SUCCESS;
}

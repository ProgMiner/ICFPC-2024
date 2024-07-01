#include <iostream>
#include <fstream>
#include <cstdint>
#include <vector>
#include <string>


static std::vector<std::string> read_pic(std::istream & f) {
    std::vector<std::string> res;

    std::string line;
    while ((f >> line)) {
        res.push_back(std::move(line));
    }

    return res;
}

int main(int argc, char * argv[]) {
    if (argc < 2) {
        return 1;
    }

    std::ifstream pic_file { argv[1] };
    auto pic = read_pic(pic_file);

    int64_t x = 0, y = 0;
    for (uint64_t i = 0; i < pic.size(); ++i) {
        for (uint64_t j = 0; j < pic[i].size(); ++j) {
            if (pic[i][j] == 'L') {
                x = j;
                y = i;
                goto cont;
            }
        }
    }

cont:
    std::string path;
    std::cin >> path;

    for (char c : path) {
        int64_t next_x = x, next_y = y;

        switch (c) {
        case 'L':
            --next_x;
            break;

        case 'R':
            ++next_x;
            break;

        case 'U':
            --next_y;
            break;

        case 'D':
            ++next_y;
            break;
        }

        if (next_x < 0 || next_y < 0) {
            continue;
        }

        if (static_cast<uint64_t>(next_y) > pic.size()) {
            continue;
        }

        if (static_cast<uint64_t>(next_x) > pic[next_y].size()) {
            continue;
        }

        if (pic[next_y][next_x] == '#') {
            continue;
        }

        pic[next_y][next_x] = '*';
        x = next_x;
        y = next_y;
    }

    pic[y][x] = 'L';

    for (auto & l : pic) {
        std::cout << l << '\n';
    }
}

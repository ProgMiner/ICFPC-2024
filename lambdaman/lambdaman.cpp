#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include <iostream>
#include <optional>
#include <cstdint>
#include <vector>
#include <string>
#include <queue>
#include <list>
#include <set>


struct node {

    uint64_t x, y;
    bool visited = false;
    std::vector<node*> edges;
};

struct path_list {

    char dir;
    const path_list * prev;
};

using field = std::unordered_map<uint64_t, std::unordered_map<uint64_t, node>>;

static node * get_node(field & nodes, uint64_t x, uint64_t y) {
    if (auto it1 = nodes.find(y); it1 != nodes.end()) {
        if (auto it2 = it1->second.find(x); it2 != it1->second.end()) {
            return &it2->second;
        }
    }

    return nullptr;
}

static char get_dir(const node & a, const node & b) {
    if (b.x < a.x) {
        return 'L';
    } else if (b.x > a.x) {
        return 'R';
    } else if (b.y < a.y) {
        return 'U';
    } else if (b.y > a.y) {
        return 'D';
    } else {
        throw std::logic_error { "wtf" };
    }
}

static node * find_next1(field & nodes, uint64_t x, uint64_t y, std::string & path) {
    std::list<path_list> path_nodes;

    std::queue<std::pair<node *, const path_list *>> q;
    q.push({ get_node(nodes, x, y), nullptr });

    std::optional<std::pair<node *, const path_list *>> res;
    std::set<std::pair<uint64_t, uint64_t>> visited;

    while (!q.empty()) {
        auto [cur, path_node] = q.front();
        q.pop();

        if (visited.find(std::make_pair(cur->x, cur->y)) != visited.end()) {
            continue;
        }

        visited.insert(std::make_pair(cur->x, cur->y));

        if (!cur->visited) {
            res = { cur, path_node };
            break;
        }

        for (node * const neigh : cur->edges) {
            path_nodes.push_back({ get_dir(*cur, *neigh), path_node });
            q.push({ neigh, &path_nodes.back() });
        }
    }

    if (!res) {
        return nullptr;
    }

    const path_list * path_node = res->second;
    while (path_node) {
        path.push_back(path_node->dir);
        path_node = path_node->prev;
    }

    std::reverse(path.begin(), path.end());
    return res->first;
}

static uint64_t find_next2_hlp(std::unordered_set<node *> & vis, node * cur) {
    if (cur->visited) {
        return 0;
    }

    if (vis.find(cur) != vis.end()) {
        return 0;
    }

    vis.insert(cur);

    uint64_t res = 1;
    for (node * const neigh : cur->edges) {
        res += find_next2_hlp(vis, neigh);
    }

    return res;
}

static node * find_next2(field & nodes, uint64_t x, uint64_t y, std::string & path) {
    node * const cur = get_node(nodes, x, y);

    uint64_t best = UINT64_MAX;
    std::vector<node *> best_nodes;
    for (node * const neigh : cur->edges) {
        if (neigh->visited) {
            continue;
        }

        std::unordered_set<node *> vis;
        const uint64_t x = find_next2_hlp(vis, neigh);
        
        if (best > x) {
            best = x;
            best_nodes = { neigh };
        } else if (best == x) {
            best_nodes.push_back(neigh);
        }
    }

    if (best_nodes.empty() || best_nodes.size() > 1) {
        return nullptr;
    }

    path += get_dir(*cur, *best_nodes.front());
    return best_nodes.front();
}

static node * find_next(field & nodes, uint64_t x, uint64_t y, std::string & path) {
    node * result;

    if ((result = find_next2(nodes, x, y, path))) {
        return result;
    }

    return find_next1(nodes, x, y, path);
}

int main() {
    field nodes;

    uint64_t n, m, x, y, all = 0;

    {
        uint64_t i = 0;
        std::string line;
        while (std::cin >> line) {
            m = line.size();

            for (std::size_t j = 0; j < m; ++j) {
                if (line[j] == 'L') {
                    x = j;
                    y = i;
                }

                if (line[j] == '#') {
                    continue;
                }

                nodes[i][j].x = j;
                nodes[i][j].y = i;
                ++all;
            }

            ++i;
        }

        n = i;
    }

    for (uint64_t y = 0; y < n; ++y) {
        for (uint64_t x = 0; x < m; ++x) {
            node * const cur = get_node(nodes, x, y);

            if (!cur) {
                continue;
            }

#define ADD_EDGE(_x, _y) \
    if (node * const neigh = get_node(nodes, _x, _y); neigh) { \
        neigh->edges.push_back(cur); \
        cur->edges.push_back(neigh); \
    }

            ADD_EDGE(x - 1, y);
            ADD_EDGE(x, y - 1);

#undef ADD_EDGE
        }
    }

    nodes[y][x].visited = true;

    node * next;
    uint64_t prog = 0;
    std::string res, tmp;
    while ((next = find_next(nodes, x, y, tmp))) {
        next->visited = true;
        x = next->x;
        y = next->y;

        std::cerr << "Progress: " << prog++ << '/' << all << '\r';
        std::cerr.flush();

        res += std::move(tmp);
        tmp.clear();
    }

    std::cout << res << std::endl;
}

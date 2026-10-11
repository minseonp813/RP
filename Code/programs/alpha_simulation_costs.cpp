// Continuous-portfolio cross-CCEI costs for the alpha simulation's ctypes driver.
// A violation is a mixed strongly connected component containing a strict edge.
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <vector>

namespace {
using Mask = uint64_t;
constexpr double tolerance = 1e-12;
constexpr double undefined_cost = 1e-9;

struct Graph {
    int n, n_individual;
    std::vector<double> ratio, grid;

    Graph(const double* data, int size, int split) :
        n(size), n_individual(split), ratio(n * n) {
        for (int u = 0; u < n; ++u) {
            const double* a = data + 4 * u;
            double own = a[1] * a[2] + a[0] * a[3];
            for (int v = 0; v < n; ++v) {
                const double* b = data + 4 * v;
                double r = (a[1] * b[2] + a[0] * b[3]) / own;
                ratio[u * n + v] = r;
            }
        }
    }

    Graph(const Graph& source, const std::vector<int>& keep, int split) :
        n(keep.size()), n_individual(split), ratio(n * n) {
        for (int u = 0; u < n; ++u)
            for (int v = 0; v < n; ++v)
                ratio[u * n + v] = source.ratio[keep[u] * source.n + keep[v]];
    }

    bool violation(double e, bool strict_as_weak = false) const {
        const int words = (n + 63) / 64;
        std::vector<Mask> reach(n * words, 0), strict(n * words, 0);
        for (int u = 0; u < n; ++u) {
            reach[u * words + u / 64] |= Mask(1) << (u % 64);
            for (int v = 0; v < n; ++v) {
                const double r = ratio[u * n + v];
                const double allowance = tolerance * std::max(std::abs(r), std::abs(e));
                const bool weak = r <= e + allowance;
                if (weak) reach[u * words + v / 64] |= Mask(1) << (v % 64);
                if (u != v && (strict_as_weak ? weak : r < e - allowance))
                    strict[u * words + v / 64] |= Mask(1) << (v % 64);
            }
        }
        for (int v = 0; v < n; ++v) {
            const int word = v / 64;
            const Mask bit = Mask(1) << (v % 64);
            for (int u = 0; u < n; ++u) if (reach[u * words + word] & bit)
                for (int w = 0; w < words; ++w)
                    reach[u * words + w] |= reach[v * words + w];
        }
        std::vector<bool> seen(n, false);
        std::vector<Mask> component(words);
        for (int u = 0; u < n; ++u) if (!seen[u]) {
            std::fill(component.begin(), component.end(), 0);
            bool individual = false, group = false;
            for (int v = 0; v < n; ++v)
                if ((reach[u * words + v / 64] & (Mask(1) << (v % 64))) &&
                    (reach[v * words + u / 64] & (Mask(1) << (u % 64)))) {
                    seen[v] = true;
                    component[v / 64] |= Mask(1) << (v % 64);
                    if (v < n_individual) individual = true; else group = true;
                }
            // split=0 requests ordinary GARP/CCEI for precision diagnostics.
            if (n_individual != 0 && !(individual && group)) continue;
            for (int v = 0; v < n; ++v)
                if (component[v / 64] & (Mask(1) << (v % 64)))
                    for (int w = 0; w < words; ++w)
                        if (strict[v * words + w] & component[w]) return true;
        }
        return false;
    }

    double cost() {
        if (!violation(1.0)) return 0.0;
        for (int u = 0; u < n; ++u) for (int v = 0; v < n; ++v) {
            const double r = ratio[u * n + v];
            if (u != v && r >= 0.0 && r < 1.0) grid.push_back(r);
        }
        std::sort(grid.begin(), grid.end());
        grid.erase(std::unique(grid.begin(), grid.end()), grid.end());
        int low = -1, high = grid.size();
        while (high - low > 1) {
            const int middle = (low + high) / 2;
            if (violation(grid[middle])) high = middle; else low = middle;
        }
        // A weak-only cycle at the last valid grid point becomes strict just
        // above that point, so the supremum is that point rather than the next.
        const bool attained = low >= 0 && violation(grid[low], true);
        const double threshold = attained ? grid[low] :
            (high == int(grid.size()) ? 1.0 : grid[high]);
        return 1.0 - threshold;
    }
};
}

extern "C" double alpha_cross_cost(const double* data, int n, int n_individual) {
    if (n <= 0 || n_individual < 0 || n_individual > n)
        return std::numeric_limits<double>::quiet_NaN();
    return Graph(data, n, n_individual).cost();
}

// members: i's n_i observations followed by j's n_i observations; groups:
// n_groups consecutive n_g-row donor datasets. out contains one I_i per donor.
extern "C" void alpha_batch_indices(const double* members, int n_i,
    const double* groups, int n_g, int n_groups, double* out) {
    const int n = 2 * n_i + n_g;
    std::vector<double> data(n * 4);
    std::copy(members, members + 8 * n_i, data.begin());
    std::vector<int> first, second;
    for (int u = 0; u < n_i; ++u) {
        first.push_back(u);
        second.push_back(n_i + u);
    }
    for (int u = 2 * n_i; u < n; ++u) {
        first.push_back(u);
        second.push_back(u);
    }
    for (int g = 0; g < n_groups; ++g) {
        std::copy(groups + 4 * n_g * g, groups + 4 * n_g * (g + 1),
            data.begin() + 8 * n_i);
        Graph pooled(data.data(), n, 2 * n_i);
        const double c_i = Graph(pooled, first, n_i).cost();
        const double c_j = Graph(pooled, second, n_i).cost();
        const double c_n = pooled.cost();
        out[g] = c_n <= undefined_cost ? std::numeric_limits<double>::quiet_NaN() :
            0.5 + (c_i - c_j) / (2.0 * c_n);
    }
}

#include <vector>
#include <string>
#include <iostream>
#include <algorithm>

using string_size = std::string::size_type;

string_size compare_strings(const std::string &source,
	const std::string &target);

int main()
{
	size_t diff = compare_strings("bla [easy] blu", "bla bli");

	std::cout << diff;
	std::cout << "\n";
}

string_size compare_strings(const std::string &source,
	const std::string &target)
{
	if (source.size() > target.size()) {
		return compare_strings(target, source);
	}

	const string_size min_size = source.size(), max_size = target.size();
	std::vector<string_size> lev_dist(min_size + 1);

	for (string_size i = 0; i <= min_size; i++) {
		lev_dist[i] = i;
	}

	for (string_size i = 1; i <= max_size; i++) {
		string_size prev = lev_dist[0]++;

		for (string_size j = 1; j <= min_size; j++) {
			string_size prev_save = lev_dist[j];

			if (source[j - 1] == target[i - 1]) {
				lev_dist[j] = prev;
			} else {
				lev_dist[j] = std::min(std::min(
					lev_dist[j - 1], lev_dist[j]
				), prev) + 1;
			}

			prev = prev_save;
		}
	}

	return lev_dist[min_size];
}

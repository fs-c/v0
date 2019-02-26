#include <string>
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;
using string_size = std::string::size_type;

string_size compare_strings(const std::string &source,
			    const std::string &target);

int main(int argc, char *argv[])
{
	if (argc < 3) {
		std::cerr << "usage: " << argv[0]
			  << " <path to directory> "
			     "<target name>"
			  << std::endl;

		return 1;
	}

	std::string path {argv[1]};

	if (!fs::exists(path)) {
		std::cerr << "the given directory (" << path
			  << ") does not exist" << std::endl;

		return 1;
	}

	std::string target {argv[2]};

	struct best_match {
		std::string name;
		string_size score;
	} best_match;

	// Let it overflow on purpose, assumes that string_size is always unsigned
	best_match.score = static_cast<string_size>(-1);

	for (const auto &entry : fs::directory_iterator(path)) {
		const auto name {entry.path().string()};
		const auto score {compare_strings(name, target)};

		std::cout << "name: '" << name << "', target: '" << target << "' = "
			<< score << std::endl;

		if (best_match.score == -1 || score < best_match.score) {
			best_match.name = name;
			best_match.score = score;
		}
	}

	std::cout << "best match: '" << best_match.name << "'" << std::endl;
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
				lev_dist[j] = std::min(std::min(lev_dist[j - 1],
								lev_dist[j]),
						       prev)
					      + 1;
			}

			prev = prev_save;
		}
	}

	return lev_dist[min_size];
}

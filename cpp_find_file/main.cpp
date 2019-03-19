#include <string>
#include <iostream>
#include <filesystem>

std::string find_file(const std::string &path, const std::string &target);

size_t compare_strings(const std::string &source, const std::string &target);

int main(int argc, char *argv[])
{
	if (argc < 3) {
		std::cerr << "usage: " << argv[0]
			  << " <path to directory> "
			     "<target name>"
			  << std::endl;

		return 1;
	}

	std::string path{argv[1]};

	if (!std::filesystem::exists(path)) {
		std::cerr << "the given directory (" << path
			  << ") does not exist" << std::endl;

		return 1;
	}

	std::string best_match = find_file(path, argv[2]);

	std::cout << "best match: '" << best_match << "'" << std::endl;
}

std::string find_file(const std::string &path, const std::string &target)
{
	struct best_match {
		size_t score;
		std::string name;
	} best_match;

	best_match.score = static_cast<size_t>(-1);

	for (const auto &entry : std::filesystem::directory_iterator(path)) {
		const auto name{entry.path().string()};
		const auto score{compare_strings(name, target)};

		std::cout << "name: '" << name << "', target: '" << target
			  << "' = " << score << std::endl;

		if (best_match.score == -1 || score < best_match.score) {
			best_match.name = name;
			best_match.score = score;
		}
	}

	return best_match.name;
}

size_t compare_strings(const std::string &base, const std::string &target)
{
	if (base.size() > target.size()) {
		return compare_strings(target, base);
	}

	const auto min_size = base.size(), max_size = target.size();
	std::vector<size_t> lev_dist(min_size + 1);

	for (size_t i = 0; i <= min_size; i++) {
		lev_dist[i] = i;
	}

	for (size_t i = 1; i <= max_size; i++) {
		auto prev = lev_dist[0]++;

		for (size_t j = 1; j <= min_size; j++) {
			const auto prev_save = lev_dist[j];

			if (base[j - 1] == target[i - 1]) {
				lev_dist[j] = prev;
			} else {
				const auto smaller =
					std::min(lev_dist[j - 1], lev_dist[j]);

				lev_dist[j] = std::min(smaller, prev) + 1;
			}

			prev = prev_save;
		}
	}

	return lev_dist[min_size];
}

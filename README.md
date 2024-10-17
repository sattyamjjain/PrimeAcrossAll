
# PrimeAcrossAll

PrimeAcrossAll is a project that implements the prime-checking algorithm in as many programming languages as possible. The goal of this project is to compare the implementations, analyze the performance, and learn the syntax and paradigms of different languages.

## Project Structure

```
    PrimeAcrossAll/
    │
    ├── languages/             # Main folder for all language implementations
    │   ├── python/            # Python implementation
    │   ├── java/              # Java implementation
    │   ├── c/                 # C implementation
    │   ├── javascript/        # JavaScript implementation
    │   └── [more languages]   # Add more language implementations
    │
    ├── scripts/               # Automation and utility scripts
    │   └── run_all.sh         # Script to run all implementations
    │   └── docker_manage.sh   # Script to build, run, and manage Docker containers
    │   └── benchmark_all.sh   # Script to build, run, and manage benchmark Docker containers
    │
    ├── Dockerfile             # Dockerfile for multi-language environment setup
    ├── README.md              # This file
    ├── LICENSE.md             # License information
    └── CONTRIBUTING.md        # Guidelines for contributing
```

## How to Run

1. Clone the repository:

   ```bash
   git clone https://github.com/sattyamjjain/PrimeAcrossAll.git
   ```

2. Navigate to the project directory:

   ```bash
   cd PrimeAcrossAll
   ```

3. Build the Docker image, which includes all language environments.:

   ```bash
   bash scripts/docker_manage.sh
   ```

4. Run the script inside the container to check prime numbers in all implemented languages.:

   ```bash
   bash scripts/docker_manage.sh run
   ```

5. Run the benchmarking script inside the container to check prime numbers in all implemented languages.:

   ```bash
   bash scripts/docker_manage.sh benchmark
   ```

## Adding a New Language

1. Create a new directory under `languages/` for the new language.
2. Implement the prime-checking algorithm in the new language.
3. Update `run_all.sh` to include your language's execution.
4. Test your implementation and submit a pull request.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

# by ChatGPT

import subprocess

# Starting point of seed
base = 100

# Number of runs
num_runs = 10

# Path to the Common Lisp script
lisp_script = "run.lisp"

# Results will be collected in a list
results = []

for seed in range(base + 1, base + num_runs + 1):
    print(f"Running with seed: {seed}")
    try:
        # Call the Common Lisp script with the current seed
        result = subprocess.run(
            ["sbcl", "--script", lisp_script, str(seed)],
            capture_output=True,
            text=True,
            check=True,
        )
        # Collect the output
        output = result.stdout
        denoised = [line for line in output.split('\n') if line.strip() != ''][-1]
        results.append((seed, denoised))
        print(f"Seed {seed} -> {denoised}")
    except subprocess.CalledProcessError as e:
        print(f"Error with seed {seed}:\n{e.stderr}")
        results.append((seed, f"Error: {e.stderr.strip()}"))

# Print final results
print("\nFinal Results:")
for seed, output in results:
    print(f"Seed {seed} -> {output}")

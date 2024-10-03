#!/bin/bash

# Run Python implementation
echo "Running Python implementation..."
python3 languages/python/prime.py << EOF
17
EOF

# Run Java implementation
echo "Running Java implementation..."
javac languages/java/Prime.java
java -cp languages/java Prime << EOF
17
EOF

# Run C implementation
echo "Running C implementation..."
gcc languages/c/prime.c -o languages/c/prime -lm
languages/c/prime << EOF
17
EOF

# Run JavaScript implementation
echo "Running JavaScript implementation..."
node languages/javascript/prime.js << EOF
17
EOF

# Placeholder for other languages

echo "All implementations executed."

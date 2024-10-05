// languages/graphql/prime.js
const { graphql, buildSchema } = require('graphql');

// Define the schema with a query for prime check
const schema = buildSchema(`
  type Query {
    isPrime(num: Int!): String
  }
`);

// Define the resolver for the isPrime query
const root = {
  isPrime: ({ num }) => {
    if (num <= 1) return `${num} is not a prime number.`;
    for (let i = 2; i <= Math.sqrt(num); i++) {
      if (num % i === 0) return `${num} is not a prime number.`;
    }
    return `${num} is a prime number.`;
  },
};

// Function to run the GraphQL query
const runQuery = async (num) => {
  const query = `{ isPrime(num: ${num}) }`;
  const response = await graphql(schema, query, root);
  console.log(response.data.isPrime);
};

// Read input number from command line
const number = process.argv[2];
runQuery(parseInt(number));

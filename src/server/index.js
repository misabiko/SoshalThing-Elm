const Twitter = require('twitter-v2');
const credentials = require('../../credentials.json');

const client = new Twitter({
  consumer_key: credentials.consumer_key,
  consumer_secret: credentials.consumer_secret,
  access_token_key: credentials.access_key,
  access_token_secret: credentials.access_secret,
});

(async () => {
    const { data } = await client.get('tweets', { ids: '1228393702244134912' });
    console.log(data);
})();
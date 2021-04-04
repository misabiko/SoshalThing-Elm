const express = require('express');
const path = require('path');
const TwitterV2 = require('twitter-v2');
const TwitterV1 = require('twitter-lite');
const credentials = require('../../credentials.json');
const favicon = require('serve-favicon');

const clientV2 = new TwitterV2({
	consumer_key: credentials.consumer_key,
	consumer_secret: credentials.consumer_secret,
	access_token_key: credentials.access_key,
	access_token_secret: credentials.access_secret,
});

const clientV1 = new TwitterV1({
	consumer_key: credentials.consumer_key,
	consumer_secret: credentials.consumer_secret,
	access_token_key: credentials.access_key,
	access_token_secret: credentials.access_secret,
});

const app = express();
const port = process.env.PORT || 5000;
const icoPath = path.join('dist', 'favicon.ico');

app.use(express.static('dist'));
app.use(favicon(icoPath));
app.use('/main.js', express.static(path.join(__dirname, 'index.js')));
app.use('/style.css', express.static(path.join(__dirname, 'style.css')));

app.use(function(err, req, res, _next) {
	console.error(err.stack)
	res.status(500).send('Something broke!')
})

const twitterRouter = express.Router();

twitterRouter.get('/home_timeline', async (req, res) => {
	const response = await clientV1.get('statuses/home_timeline', {
		tweet_mode: 'extended',
		...(req.query),
	});

	res.json(response);
});

twitterRouter.get('/user_timeline', async (req, res) => {
	const response = await clientV1.get('statuses/user_timeline', {
		tweet_mode: 'extended',
		...(req.query),
	});

	res.json(response);
});

twitterRouter.get('/list', async (req, res) => {
	const response = await clientV1.get('lists/statuses', {
		tweet_mode: 'extended',
		...(req.query),
	});

	res.json(response);
});

twitterRouter.get('/search', async (req, res) => {
	const response = await clientV1.get('search/tweets', {
		tweet_mode: 'extended',
		...(req.query),
	});

	res.json(response);
});

app.use('/twitter', twitterRouter);

app.listen(port, () => console.log(`Listening at http://localhost:${port}`))
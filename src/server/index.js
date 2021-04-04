const express = require('express');
const path = require('path');
const TwitterV2 = require('twitter-v2');
const TwitterV1 = require('twitter-lite');
const credentials = require('../../credentials.json');
const favicon = require('serve-favicon');
const morgan = require('morgan');

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
app.use(morgan('dev'));
app.use('/main.js', express.static(path.join(__dirname, 'index.js')));
app.use('/style.css', express.static(path.join(__dirname, 'style.css')));

app.use(function (err, req, res, _next) {
	console.error(err.stack)
	res.status(500).send('Something broke!')
})

function parseQueryErrors(e, next) {
	if ('errors' in e) {
		for (const error of e.errors)
			console.error(error);
	} else
		console.error(e);

	next(e);
}

app.route('/twitter/v1/:endpoint1/:endpoint2')
	.get(async (req, res, next) => {
		try {
			const response = await clientV1.get(`${req.params.endpoint1}/${req.params.endpoint2}`, {
				...(req.query),
			});

			res.json(response);
		} catch (e) {
			parseQueryErrors(e, next);
		}
	})
	.post(async (req, res, next) => {
		try {
			const response = await clientV1.post(`${req.params.endpoint1}/${req.params.endpoint2}`, {
				...(req.query),
			});

			res.json(response);
		} catch (e) {
			parseQueryErrors(e, next);
		}
	});

if (process.platform === "win32") {
	var rl = require("readline").createInterface({
		input: process.stdin,
		output: process.stdout
	});

	rl.on("SIGINT", function () {
		process.emit("SIGINT");
	});
}

process.on("SIGINT", function () {
	//graceful shutdown
	process.exit();
});

app.listen(port, () => console.log(`Listening at http://localhost:${port}`))
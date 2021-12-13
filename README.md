# SoshalThing-Elm

An Elm port of SoshalThing, a TweetDeck-style app to support various services (though only Twitter on this version). <br />
At the time of writing, the main version of SoshalThing is on Vue 3: https://github.com/misabiko/SoshalThing

---

Serve the app with: `npm run server` <br />
(might not work on the current commit)

---

Elm is very fun to work with, with minimal dependencies and both the app and proxy are served on the same expressjs web server. <br />
But the type system wasn't flexible/strong enough to support the multiple services and article types architecture that SoshalThing needs.

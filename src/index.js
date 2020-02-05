import { Elm } from "./app/Main.elm";

const LOCALHOST_URI = "http://localhost:5010";

const PROD_URI = "https://api.bejebeje.com";

const API_ROOT_URI =
  process.env.NODE_ENV === "development" ? LOCALHOST_URI : PROD_URI;

const app = Elm.Main.init({
  node: document.getElementById("elm-root"),
  flags: { apiRootUrl: API_ROOT_URI }
});

input.onfocus = function() {
  window.scrollTo(0, 0);
  document.body.scrollTop = 0;
};

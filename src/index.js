import { Elm } from "./app/Main.elm";

const LOCALHOST_URI = "http://localhost:5010";

const PROD_URI = "https://api.bejebeje.com";

const API_ROOT_URI =
  process.env.NODE_ENV === "development" ? LOCALHOST_URI : PROD_URI;

const app = Elm.Main.init({
  node: document.getElementById("elm-root"),
  flags: { apiRootUrl: API_ROOT_URI }
});

// First we get the viewport height and we multiple it by 1% to get a value for a vh unit
let vh = window.innerHeight * 0.01;

// Then we set the value in the --vh custom property to the root of the document
document.documentElement.style.setProperty("--vh", `${vh}px`);

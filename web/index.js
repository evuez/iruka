import css from './style.css';
import { Elm } from "./src/Main.elm";


const basePath = new URL(document.baseURI).pathname;

const app = Elm.Main.init({
  node: document.querySelector("#app"),
  flags: { basePath }
});

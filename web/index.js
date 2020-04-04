import css from './style.css';
import { Elm } from "./src/Main.elm";


const storedScores = localStorage.getItem("scores");
const scores = storedScores ? JSON.parse(storedScores) : [];
const basePath = new URL(document.baseURI).pathname;

const app = Elm.Main.init({
  node: document.querySelector("#app"),
  flags: { basePath, scores }
});

app.ports.saveScores.subscribe((data) => localStorage.setItem("scores", JSON.stringify(data)));

// Styles
require('./styles/main.scss');

// Elm
const Elm = require('../elm/Main');
Elm.Main.embed(document.getElementById('main'));

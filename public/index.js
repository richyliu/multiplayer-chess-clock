if (process.env.NODE_ENV === 'production') {
  require('../out.js');
} else {
  require('../output/Main/index.js').main();
}


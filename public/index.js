if (process.env.NODE_ENV === 'production') {
  require('../out_purs.js');
} else {
  require('../output/Main/index.js').main();
}

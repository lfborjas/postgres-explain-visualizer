// From: https://cli.vuejs.org/guide/html-and-static-assets.html#disable-index-generation
module.exports = {
  // disable hashes in filenames
  filenameHashing: false,
  outputDir: '../static/pev2',
  // delete HTML related webpack plugins
  chainWebpack: config => {
    //config.plugins.delete('html')
    config.plugins.delete('preload')
    config.plugins.delete('prefetch')
  }
}

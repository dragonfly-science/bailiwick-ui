const path = require('path');

module.exports = {
    entry: path.join(__dirname, 'js-src/index.js'),
    output: {
        path: path.join(__dirname, 'static'),
        filename: 'bailiwick.bundle.js'
    },
    module: {
        rules: [{
            test: /\.js/,
            exclude: /(node_modules)/,
            use: [{
                loader: 'babel-loader'
            }]
        }]
    },
    stats: {
        colors: true
    },
    devtool: 'source-map'
};
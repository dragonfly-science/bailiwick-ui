var path = require('path');

module.exports = {
    mode: 'none',
    entry: './js/main.js',
    output: {
        path: path.resolve(__dirname, "static"),
        filename: 'bailiwick.min.js'
    },
    module: {
        rules: [
            { 
                test: path.join(__dirname, 'js'),
                loader: 'babel-loader'
            }
        ]
    }
};
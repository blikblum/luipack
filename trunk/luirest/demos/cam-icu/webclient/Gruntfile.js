'use strict';
var SERVER_PORT = 9000;
var mountFolder = function (connect, dir) {
    return connect.static(require('path').resolve(dir));
};

// # Globbing
// for performance reasons we're only matching one level down:
// 'test/spec/{,*/}*.js'
// use this if you want to match all subfolders:
// 'test/spec/**/*.js'
// templateFramework: 'handlebars'

module.exports = function (grunt) {
    // show elapsed time at the end
    require('time-grunt')(grunt);
    // load all grunt tasks
    require('load-grunt-tasks')(grunt);

    // configurable paths
    var yeomanConfig = {
        app: 'app',
        dist: 'dist'
    };

    grunt.initConfig({
        browserSync: {
            dev: {
                bsFiles: {
                    src : ['app/scripts/**/*.js', 'app/*.js']
                },
                options: {
                    proxy: "localhost/camicu/app"
                }
            }
        },
        yeoman: yeomanConfig,
        watch: {
            options: {
                nospawn: true
            },
            compass: {
                files: ['<%= yeoman.app %>/styles/{,*/}*.{scss,sass}'],
                tasks: ['compass']
            },
            handlebars: {
                files: [
                    '<%= yeoman.app %>/scripts/templates/*.hbs'
                ],
                tasks: ['handlebars']
            },
            test: {
                files: ['<%= yeoman.app %>/scripts/{,*/}*.js', 'test/spec/**/*.js'],
                tasks: ['test']
            }
        },
        open: {
            server: {
                path: 'http://localhost:<%= connect.options.port %>'
            }
        },
        clean: {
            dist: ['.tmp', '<%= yeoman.dist %>/*'],
            server: '.tmp'
        },
        jshint: {
            options: {
                jshintrc: '.jshintrc',
                reporter: require('jshint-stylish')
            },
            all: [
                'Gruntfile.js',
                '<%= yeoman.app %>/scripts/{,*/}*.js',
                '!<%= yeoman.app %>/scripts/vendor/*',
                'test/spec/{,*/}*.js'
            ]
        },
        mocha: {
            all: {
                options: {
                    run: true,
                    urls: ['http://localhost:<%= connect.test.options.port %>/index.html']
                }
            }
        },
        sass: {
          all: {
              files: [{
                  expand: true,
                  cwd: '<%= yeoman.app %>/styles',
                  src: ['*.scss'],
                  dest: '<%= yeoman.app %>/styles',
                  ext: '.css'

              }]
          },
          options: {
              includePaths: ['<%= yeoman.app %>/bower_components']
          }
        },
        requirejs: {
            dist: {
                // Options: https://github.com/jrburke/r.js/blob/master/build/example.build.js
                options: {
                    baseUrl: '<%= yeoman.app %>/scripts',
                    optimize: 'none',
                    paths: {
                        'templates': '../../app/scripts/templates',
                        'jquery': '../../app/bower_components/jquery/jquery',
                        'underscore': '../../app/bower_components/underscore/underscore',
                        'backbone': '../../app/bower_components/backbone/backbone',
                        'bootstrap': '../../app/bower_components/bootstrap-sass-official/assets/javascripts/bootstrap',
                        'handlebars': '../../app/bower_components/handlebars/handlebars',
                        'stickit': '../../app/bower_components/backbone.stickit/backbone.stickit',
                        'stickitform': '../../app/bower_components/backbone.stickit.form/src/backbone.stickit.form',
                        'text': '../../app/bower_components/requirejs-text/text',
                        'validation': '../../app/bower_components/backbone-validation/dist/backbone-validation-amd',
                        'maskedinput': '../../app/bower_components/jquery.maskedinput/dist/jquery.maskedinput',
                        'localstorage': '../../app/bower_components/backbone.localstorage/backbone.localStorage',
                        'computed': '../../app/bower_components/backbone.computedfields/lib/amd/backbone.computedfields'
                    },
                    // TODO: Figure out how to make sourcemaps work with grunt-usemin
                    // https://github.com/yeoman/grunt-usemin/issues/30
                    //generateSourceMaps: true,
                    // required to support SourceMaps
                    // http://requirejs.org/docs/errors.html#sourcemapcomments
                    preserveLicenseComments: false,
                    useStrict: true
                    //uglify2: {} // https://github.com/mishoo/UglifyJS2
                }
            }
        },
        useminPrepare: {
            html: '<%= yeoman.app %>/index.html',
            options: {
                dest: '<%= yeoman.dist %>'
            }
        },
        usemin: {
            html: ['<%= yeoman.dist %>/{,*/}*.html'],
            css: ['<%= yeoman.dist %>/styles/{,*/}*.css'],
            options: {
                dirs: ['<%= yeoman.dist %>']
            }
        },
        imagemin: {
            dist: {
                files: [{
                    expand: true,
                    cwd: '<%= yeoman.app %>/images',
                    src: '{,*/}*.{png,jpg,jpeg}',
                    dest: '<%= yeoman.dist %>/images'
                }]
            }
        },
        cssmin: {
            dist: {
                files: {
                    '<%= yeoman.dist %>/styles/main.css': [
                        '.tmp/styles/{,*/}*.css',
                        '<%= yeoman.app %>/styles/{,*/}*.css'
                    ]
                }
            }
        },
        htmlmin: {
            dist: {
                options: {
                    /*removeCommentsFromCDATA: true,
                    // https://github.com/yeoman/grunt-usemin/issues/44
                    //collapseWhitespace: true,
                    collapseBooleanAttributes: true,
                    removeAttributeQuotes: true,
                    removeRedundantAttributes: true,
                    useShortDoctype: true,
                    removeEmptyAttributes: true,
                    removeOptionalTags: true*/
                },
                files: [{
                    expand: true,
                    cwd: '<%= yeoman.app %>',
                    src: '*.html',
                    dest: '<%= yeoman.dist %>'
                }]
            }
        },
        copy: {
            dist: {
                files: [{
                    expand: true,
                    dot: true,
                    cwd: '<%= yeoman.app %>',
                    dest: '<%= yeoman.dist %>',
                    src: [
                        '*.{ico,txt}',
                        '.htaccess',
                        'images/{,*/}*.{webp,gif}',
                        'styles/fonts/{,*/}*.*',
                        'bower_components/sass-bootstrap/fonts/*.*'
                    ]
                }]
            },
            html: {
                files: [{
                    expand: true,
                    cwd: '<%= yeoman.app %>',
                    dest: '.tmp/',
                    src: 'scripts/templates/*.html'
                }]
            }
        },
        bower: {
            all: {
                rjsConfig: '<%= yeoman.app %>/scripts/main.js'
            }
        },
        handlebars: {
            compile: {
                options: {
                    namespace: 'JST',
                    amd: true
                },
                files: {
                    '<%= yeoman.app %>/scripts/templates.js': ['<%= yeoman.app %>/scripts/templates/*.hbs']
                }
            }
        },
        rev: {
            dist: {
                files: {
                    src: [
                        '<%= yeoman.dist %>/scripts/{,*/}*.js',
                        '<%= yeoman.dist %>/styles/{,*/}*.css',
                        '<%= yeoman.dist %>/images/{,*/}*.{png,jpg,jpeg,gif,webp}',
                        '/styles/fonts/{,*/}*.*',
                        'bower_components/sass-bootstrap/fonts/*.*'
                    ]
                }
            }
        }
    });

    grunt.registerTask('createDefaultTemplate', function () {
        grunt.file.write('.tmp/scripts/templates.js', 'this.JST = this.JST || {};');
    });

    grunt.registerTask('test', [
        'clean:server',
        'createDefaultTemplate',
        'handlebars',
        'connect:test',
        'mocha',
        'watch:test'
    ]);

    grunt.registerTask('build', [
        'clean:dist',
        'createDefaultTemplate',
        'handlebars',
        'sass',
        'useminPrepare',
        'copy:html',
        'requirejs',
        'imagemin',
        'htmlmin',
        'concat',
        'cssmin',
        //'uglify',
        'copy:dist',
        'rev',
        'usemin'
    ]);

    grunt.registerTask('all', [
        'jshint',
        'test',
        'build'
    ]);
    grunt.registerTask('default', ["browserSync", "watch"]);
};

module.exports = {
  extends: ['@commitlint/config-conventional'],
  rules: {
    'type-enum': [
      2,
      'always',
      [
        'feat',
        'fix',
        'perf',
        'refactor',
        'BREAKING',
        'docs',
        'style',
        'chore',
        'test',
        'build',
        'ci'
      ]
    ]
  }
};

include apt

# Update Apt before installing packages
Package {
  require => Class['apt::update']
}

package { 'python-software-properties':
  ensure  => latest,
  # Drop apt::update from Package default, to avoid a cyclic dependency on
  # apt::update, because apt::update in turn will depend on this package,
  # because it has the tools to add PPAs
  require => [],
  before  => Class['apt'],
}


# Actual stuff we need..

apt::ppa { ['ppa:cassou/emacs']: }

package { ['emacs24-common', 'emacs24-bin-common', 'emacs24-nox']:
  ensure  => latest,
  require => Apt::Ppa['ppa:cassou/emacs']
}

$cask_version = '0.7.2'
$cask_archive = "cask-${cask_version}"

archive { $cask_archive:
  ensure        => present,
  digest_type   => 'sha1',
  digest_string => '2c8012487f06c6b4f47ce56bd021bb71753f1bd0',
  url           => "https://github.com/cask/cask/archive/v${cask_version}.tar.gz",
  target        => '/opt/',
}

file { '/usr/local/bin/cask':
  ensure  => link,
  target  => "/opt/${cask_archive}/bin/cask",
  require => Archive[$cask_archive],
}

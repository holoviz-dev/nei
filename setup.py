from setuptools import setup

setup_args = dict(
    name='nei',
    version='0.0.3',
    description='Notebook Emacs Integration',
    platforms=['Windows', 'Mac OS X', 'Linux'],
    license='BSD',
    provides=["nei"],
    include_package_data = True,
    python_requires=">=3.5")

if __name__=="__main__":
    setup(**setup_args)

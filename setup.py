from setuptools import setup

install_requires = [
    'jupyter_client==5.2.4',
    'nbformat',
    'nbconvert',
    'tornado',
    'ipykernel'
]

extras_require = {
    'pyviz': [
        'bokeh',
        'holoviews',
        'cssutils'
    ]
}

setup_args = dict(
    name='nei',
    version='0.0.6',
    description='Notebook Emacs Interface',
    platforms=['Mac OS X', 'Linux'],
    license='BSD',
    provides=["nei"],
    packages=["nei"],
    include_package_data = True,
    python_requires=">=3.5",
    install_requires=install_requires,
    extras_require=extras_require,
    classifiers=[
        "License :: OSI Approved :: BSD License",
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "Natural Language :: English"
    ]
)

if __name__=="__main__":
    setup(**setup_args)

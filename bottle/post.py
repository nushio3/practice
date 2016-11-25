# -*- coding:utf-8 -*-

from bottle import route, run
from bottle import get, post, request


@route('/submit')
def submit():
    return '''
    <form action="/submit" method="post">
    <p>
            Username: <input name="jobname" type="text" value=""/>
    <p>
    <textarea name="script" rows="20" cols="80"></textarea>
    <p>
            <input value="submit" type="submit" />
        </form>
    '''


@route('/submit', method='POST')
def do_submit():
    jobname = request.forms.get('jobname')
    script_str = request.forms.get('script')

    return "{} {}".format(jobname, script_str)


run(host='localhost', port=8080, debug=True)

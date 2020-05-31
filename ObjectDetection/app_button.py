import os
from flask import Flask, request, render_template, url_for, redirect
import subprocess

app = Flask(__name__)

@app.route("/")
def fileFrontPage():
    return render_template('fileform_button.html')

@app.route("/handleUpload", methods=['POST'])
def handleFileUpload():
    if 'photo' in request.files:
        photo = request.files['photo']
        if photo.filename != '':
            photo.save(os.path.join('temp/',photo.filename))
    return redirect(url_for('fileFrontPage'))


@app.route("/handleDetect", methods=['POST'])
def handleDetect():
    #import object_detection_yolo.py
    fname = os.listdir("temp/")[0]
    subprocess.call('python3 object_detection_yolo.py --image=temp/'+fname,shell = True)



    return redirect(url_for('fileFrontPage'))


if __name__ == '__main__':
    app.run(debug = True)

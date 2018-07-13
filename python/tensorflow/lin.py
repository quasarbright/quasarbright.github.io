from __future__ import absolute_import

import numpy as np
import tensorflow as tf
rng = np.random


# the function the model will try to guess
def f(x):
    return 3 * x + 1


# hyper parameters
learning_rate = 0.01
training_epochs = 1000
display_step = 50


# training data
x_train = np.linspace(-10, 10, 100)
y_train = np.asarray([f(x) for x in x_train])
n_samples = x_train.shape[0]


# graph input
x_in = tf.placeholder('float', name='x_input')
y_true = tf.placeholder('float', name='y_input')


# model weights
w = tf.Variable(rng.randn(), name='weight')
b = tf.Variable(rng.randn(), name='bias')


# linear model
y_guess = tf.add(tf.multiply(w, x_in), b)


# configure training
cost = tf.reduce_sum(tf.pow(y_true - y_guess, 2)) / (2 * n_samples)
optimizer = tf.train.GradientDescentOptimizer(learning_rate).minimize(cost)


# initialize everything
init = tf.global_variables_initializer()


# start training
with tf.Session() as sess:
    sess.run(init)

    for epoch in range(training_epochs):
        for (x,y) in zip(x_train, y_train):
            sess.run(optimizer,feed_dict={x_in:x,y_true:y})

        if epoch % display_step == 0:
            c = sess.run(cost,feed_dict={x_in:x,y_true:y})
            print('epoch {0}, cost={1:.4f}, w={2:.4f}, b={3:.4f}'.format(epoch, c, sess.run(w), sess.run(b)))

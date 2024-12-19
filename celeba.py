# Gender perdiction from image - celeb_a dataset, CNN
# Aleksey Shipitsyn, 
# 2024-12-19
# Based on: "Python Machine Learning: Machine Learning and Deep Learning with Python,
#            scikit-learn, and TensorFlow 2", 3rd Edition, 2019

import tensorflow as tf
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import pathlib

# make celeb_a dataset manually
import gdown
url = "https://drive.google.com/uc?id=1m8-EBPgi5MRubrm6iQjafK2QMHDBMSfJ"
output = "/Users/alekseyshipitsyn/tensorflow_datasets/celeba.zip" 
gdown.download(url, output, quiet=False)

import zipfile
with zipfile.ZipFile(
        "/Users/alekseyshipitsyn/tensorflow_datasets/celeba.zip", "r") as zip_ref:
    zip_ref.extractall("/Users/alekseyshipitsyn/tensorflow_datasets/")

with zipfile.ZipFile(
        "/Users/alekseyshipitsyn/tensorflow_datasets/celeba/img_align_celeba.zip", "r") as zip_ref:
    zip_ref.extractall("/Users/alekseyshipitsyn/tensorflow_datasets/celeba")

# labels and split
imgdir_path = pathlib.Path(
    '/Users/alekseyshipitsyn/tensorflow_datasets/celeba/img_align_celeba')
attr_path = pathlib.Path(
    '/Users/alekseyshipitsyn/tensorflow_datasets/celeba/list_attr_celeba.txt')
split_path = pathlib.Path(
    '/Users/alekseyshipitsyn/tensorflow_datasets/celeba/list_eval_partition.txt')

df_attr = pd.read_csv(attr_path, skiprows=1, sep='\s+', 
                      engine='python', usecols=['Male'])
df_split = pd.read_csv(split_path, sep=' ', index_col=0, names=['set'])
df_split['set_name'] = 'train'
df_split.loc[df_split['set'] == 1, 'set_name'] = 'valid'
df_split.loc[df_split['set'] == 2, 'set_name'] = 'test'
df_attr = pd.merge(df_attr, df_split[['set_name']], 
                   left_index=True, right_index=True) 
df_attr = df_attr.reset_index()
df_attr.columns = ['path', 'male', 'set']
df_attr['path'] = df_attr['path'].apply(
    lambda x: str(pathlib.Path(imgdir_path, x))) 
df_attr.loc[df_attr['male'] < 0, 'male'] = 0
df_attr.isna().sum()

ds_files_labels_train = tf.data.Dataset.from_tensor_slices(
    (df_attr.loc[df_attr['set']=='train', 'path'], 
     df_attr.loc[df_attr['set']=='train', 'male']) )
ds_files_labels_valid = tf.data.Dataset.from_tensor_slices(
    (df_attr.loc[df_attr['set']=='valid', 'path'], 
     df_attr.loc[df_attr['set']=='valid', 'male']) )
ds_files_labels_test = tf.data.Dataset.from_tensor_slices(
    (df_attr.loc[df_attr['set']=='test', 'path'], 
     df_attr.loc[df_attr['set']=='test', 'male']) )

ds_files_labels_train = ds_files_labels_train.take(16000)
ds_files_labels_valid = ds_files_labels_valid.take(1000)

def load_images(path, label):
    image = tf.io.read_file(path)
    image = tf.image.decode_jpeg(image, channels=3)
    return image, label

celeba_train = ds_files_labels_train.map(load_images)
celeba_valid = ds_files_labels_valid.map(load_images)
celeba_test = ds_files_labels_test.map(load_images)

tf.random.set_seed(1)
fig = plt.figure(figsize=(14, 12))
for i, example in enumerate(celeba_train.take(3)):
    image = example[0]
    ax = fig.add_subplot(3, 4, i*4 + 1)
    ax.imshow(image)
    if i == 0:
        ax.set_title('Original', size=15)
    
    ax = fig.add_subplot(3, 4, i*4 + 2)
    img_crop = tf.image.random_crop(image, size=(178, 178, 3)) 
    ax.imshow(img_crop)
    if i ==0:
        ax.set_title('Step 1: random crop', size=15)
    
    ax = fig.add_subplot(3, 4, i*4 + 3)
    img_flip = tf.image.random_flip_left_right(img_crop) 
    ax.imshow(img_flip)
    if i ==0:
        ax.set_title('Step 2: random flip', size=15)
    
    ax = fig.add_subplot(3, 4, i*4 + 4)
    img_resize = tf.image.resize(img_flip, size=(128, 128)) 
    ax.imshow(tf.cast(img_resize, tf.uint8))
    if i ==0:
        ax.set_title('Step 3: resize', size=15)
plt.show()


def preprocess_image(image, label, size=(64, 64), mode='train'):
    if mode == 'train':
        img_cropped = tf.image.random_crop(image, size=(178, 178, 3)) 
        img_resized = tf.image.resize(img_cropped, size=size) 
        img_flipped = tf.image.random_flip_left_right(img_resized) 
        return img_flipped / 255.0, tf.cast(label, tf.int32)
    else:
        img_cropped = tf.image.crop_to_bounding_box(
            image, offset_height=20, offset_width=0, 
            target_height=178, target_width=178) 
        img_resized = tf.image.resize(img_cropped, size=size) 
        return img_resized / 255.0, tf.cast(label, tf.int32)
    
    
tf.random.set_seed(1)
ds = celeba_train.shuffle(1000, reshuffle_each_iteration=False)
ds = ds.take(2).repeat(5)
ds = ds.map(lambda img, label: 
                preprocess_image(img, label, size=(178, 178), mode='train'))

fig = plt.figure(figsize=(15, 6))
for j, example in enumerate(ds):
    ax = fig.add_subplot(2, 5, j//2 + (j%2)*5+1)
    ax.set_xticks([])
    ax.set_yticks([])
    ax.imshow(example[0])
    ax.set_title(f'Label {example[1]}')
plt.show()
  
# train and validation data sets  
BATCH_SIZE = 32
BUFFER_SIZE = 1000
IMAGE_SIZE = (64, 64)
steps_per_epoch = np.ceil(16000/BATCH_SIZE)
ds_train = celeba_train.map(
    lambda img, label: 
        preprocess_image(img, label, size=IMAGE_SIZE, mode='train'))    
ds_train = ds_train.shuffle(buffer_size=BUFFER_SIZE).repeat()
ds_train = ds_train.batch(BATCH_SIZE)    
ds_valid = celeba_valid.map(
    lambda img, label: 
        preprocess_image(img, label, size=IMAGE_SIZE, mode='eval'))    
ds_valid = ds_valid.batch(BATCH_SIZE)    
    
# CNN model
model = tf.keras.Sequential([
    tf.keras.layers.Conv2D(
        filters=32, kernel_size=(3, 3), padding='same', activation='relu'),
    tf.keras.layers.MaxPooling2D(pool_size=(2, 2)),
    tf.keras.layers.Dropout(rate=0.5),                                 

    tf.keras.layers.Conv2D(
        filters=64, kernel_size=(3, 3), padding='same', activation='relu'),
    tf.keras.layers.MaxPooling2D(pool_size=(2, 2)),
    tf.keras.layers.Dropout(rate=0.5),                                 

    tf.keras.layers.Conv2D(
        filters=128, kernel_size=(3, 3), padding='same', activation='relu'),
    tf.keras.layers.MaxPooling2D(pool_size=(2, 2)),

    tf.keras.layers.Conv2D(
        filters=256, kernel_size=(3, 3), padding='same', activation='relu'),

    tf.keras.layers.GlobalAveragePooling2D(),
    tf.keras.layers.Dense(units=1, activation=None)    
])

# fit model
tf.random.set_seed(1)
model.build(input_shape=(None, 64, 64, 3))
model.summary()
model.compile(optimizer=tf.keras.optimizers.Adam(),
              loss=tf.keras.losses.BinaryCrossentropy(from_logits=True), 
              metrics='accuracy')
history = model.fit(ds_train, validation_data=ds_valid, 
                    epochs=20, steps_per_epoch=steps_per_epoch)

def plot_history(history, metric='accuracy'):
    hist = history.history
    x_arr = np.arange(len(hist['loss'])) + 1
    fig = plt.figure(figsize=(12, 4))
    ax = fig.add_subplot(1, 2, 1)
    ax.plot(x_arr, hist['loss'], '-o', label='Train loss')
    ax.plot(x_arr, hist['val_loss'], '--<', label='Valid loss')
    ax.legend(fontsize=15)
    ax.set_xlabel('Epoch', size=15)
    ax.set_ylabel('Loss', size=15)
    ax = fig.add_subplot(1, 2, 2)
    ax.plot(x_arr, hist[metric], '-o', label='Train accuracy')
    ax.plot(x_arr, hist[f'val_{metric}'], '--<', label=f'Valid {metric}')
    ax.legend(fontsize=15)
    ax.set_xlabel('Epoch', size=15)
    ax.set_ylabel('Loss', size=15)
    plt.show()

plot_history(history, metric='accuracy')

# train more epochs
history = model.fit(ds_train, validation_data=ds_valid, 
                    epochs=30, initial_epoch=20, 
                    steps_per_epoch=steps_per_epoch)

plot_history(history, metric='accuracy')

# predict on test set
ds_test = celeba_test.map(
    lambda img, label:
        preprocess_image(img, label, size=IMAGE_SIZE, mode='eval'))
    
test_results = model.evaluate(ds_test.batch(32))
print('Test accuracy: {:.2f}%'.format(test_results[1]*100))
# Test accuracy: 94.83%

# visualize actual labels against predicted
ds = ds_test.take(10)
pred_logits = model.predict(ds.batch(10))
probas = tf.sigmoid(pred_logits)
probas = probas.numpy().flatten()*100

fig = plt.figure(figsize=(15, 7))
for j, example in enumerate(ds):
    ax = fig.add_subplot(2, 5, j+1)
    ax.set_xticks([])
    ax.set_yticks([])
    ax.imshow(example[0])
    if example[1].numpy() == 1:
        label = 'man'
    else:
        label = 'woman'
    ax.text(
        0.5, -0.15, 'Label: {:s}\nProb(man)={:.0f}%'.format(label, probas[j]),
        size=16, horizontalalignment='center', verticalalignment='center',
        transform=ax.transAxes)
    plt.tight_layout()
plt.show()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
import bpy
import os

# https://docs.blender.org/api/current/bpy.types.BlendData.html#bpy.types.BlendData
# To run the python script in blender: blender --python /path/to/my_script.py
# These files you've written to are still empty.
# Need to confirm you can properly copy objects across files before moving on

# Load the data from a file
original_blend_file = "./blend-data-files/sample-image-for-data-collection.blend"
lame_copy_blend_file = "./blend-data-files/lame-copy.blend"
hand_copied_blend_file = "./blend-data-files/hand-copied.blend"

bpy.data.libraries.load(original_blend_file)

# print all object names
for obj in bpy.data.objects:
    print(obj.name)
# objects have many properties, such as layers, location, dimensions, etc.
    print(obj.dimensions)
    print(obj.location)

#lame copy
data_blocks = set(bpy.data.objects)
bpy.data.libraries.write(lame_copy_blend_file, data_blocks)

# hand copy
# there are methods for constructing all of the different types of entities in blender,
# they can take specific params to render them in a particular way
for obj in data_blocks:
    if obj.name == 'Cube':
        bpy.ops.mesh.primitive_cube_add(
            location=obj.location,
            rotation=obj.delta_rotation_euler,
            layers=obj.layers
        )
        # TODO: look up the blender context
        new_cube = bpy.context.object
        print('new cube', new_cube)
        cube_set = set([new_cube])
        bpy.data.libraries.write(hand_copied_blend_file, cube_set)

# pop mutates the set. can use a while loop to pop all items
print('popping from set')
while data_blocks:
    print(data_blocks.pop().name)

# need a way to intelligently grab all the data from the blend file and
# get the most useful data about each entity (not everything will be an object type)
# Also need to select the best code to reconstruct each entity

def simulate(t, n):
	print "Sender partitions at ", t
	# How does receiver respond to m2
	m2 = 1/float(3) - t**2 * (1 - 2/float(3) * t)
	print "Receiver responds to m2 with ", m2
	# How does receiver respond to m1
	m1 = t**2 * (1 - 2/float(3) * t)
	print "Receiver respond to m1 with ", m1

	t = (m2 + m1) / float(2)
	print "Sender now partitions at ", t

	# Step through reasoning for n rounds
	#for i in range(n - 1):
	#	simulate(t, n - 1)

simulate(.1, 22)	

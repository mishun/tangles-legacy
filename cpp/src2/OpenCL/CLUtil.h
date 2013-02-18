#pragma once

#include <vector>
#include <string>
#include <stdexcept>
#include <CL/OpenCL.h>

namespace CL
{
	class Exception : public std::exception
	{
	public:
		Exception(const char *, int);
		virtual ~Exception() throw();
		virtual const char * what() const throw();

	protected:
		static std::string parseErrorCode(int);

	protected:
		std::string msg;
	};

	class NDRange
	{
	public:
		NDRange(size_t x) : _dim(1) { _range[0] = x; }
		NDRange(size_t x, size_t y) : _dim(2) { _range[0] = x; _range[1] = y; }
		NDRange(size_t x, size_t y, size_t z) : _dim(3) { _range[0] = x; _range[1] = y; _range[2] = z; }

		size_t dim() const { return _dim; }
		const size_t * range() const { return _range; }

	protected:
		size_t _dim;
		size_t _range[3];
	};

	class DeviceID
	{
	public:
		DeviceID(cl_device_id d = 0)
			: device(d)
		{
		}

		cl_device_id get() const { return device; }

	public:
		std::string getName() const;
		std::string getVendor() const;
		std::string getVersion() const;

	private:
		cl_device_id device;
	};

	class PlatformID
	{
	public:
		PlatformID(cl_platform_id p = 0)
			: platform(p)
		{
		}

		cl_platform_id get() const { return platform; }

	public:
		std::string getProfile() const;
		std::string getVersion() const;
		std::string getName() const;
		std::string getVendor() const;
		std::string getExtensions() const;
		std::vector<DeviceID> getDeviceIDs(cl_device_type);

	private:
		cl_platform_id platform;
	};

	class ContextProperties
	{
	public:
		ContextProperties();
		const cl_context_properties * get();
		void setPlatform(const PlatformID &);

	private:
		bool platform_defined;
		PlatformID platform;
		std::vector<cl_context_properties> data;
	};

	class Event
	{
	public:
		Event(cl_event _event = 0)
			: event(_event)
		{
		}

		void waitFor() const;
		void retain() const;
		void release();

	private:
		cl_event event;
	};

	class Buffer
	{
	protected:
		Buffer() {}
		virtual ~Buffer() {}

	public:
		virtual size_t getSize() const = 0;
		virtual void * getHostPtr() const = 0;
		virtual size_t getMapCount() const = 0;
		virtual size_t getReferenceCount() const = 0;
		virtual void retain() const = 0;
	};

	class Kernel
	{
	protected:
		Kernel() {}
		virtual ~Kernel() {}

	public:
		virtual std::string getFunctionName() const = 0;
		virtual size_t getNumArgs() const = 0;
		virtual size_t getReferenceCount() const = 0;
		virtual void setArg(size_t, size_t, const void *) const = 0;
		virtual void retain() const = 0;
	};

	class Program
	{
	protected:
		Program() {}
		virtual ~Program() {}

	public:
		virtual std::string getSource() const = 0;
		virtual size_t getReferenceCount() const = 0;
		virtual void build(const char *) = 0;
		virtual const Kernel * getKernel(const char *) = 0;
		virtual void retain() const = 0;
	};

	class CommandQueue
	{
	protected:
		CommandQueue() {}
		virtual ~CommandQueue() {}

	public:
		virtual size_t getReferenceCount() const = 0;
		virtual Event enqueueKernel(const Kernel &, const NDRange &, const NDRange &) const = 0;
		virtual Event enqueueTask(const Kernel &) const = 0;
		virtual Event enqueueReadBuffer(const Buffer & buffer, bool blocking, size_t offset, size_t size, void * memory) const = 0;
		virtual Event enqueueWriteBuffer(const Buffer & buffer, bool blocking, size_t offset, size_t size, const void * memory) const = 0;
		virtual Event enqueueCopyBuffer(const Buffer & src, const Buffer & dst, size_t src_offset, size_t dst_offset, size_t size) const = 0;
		virtual Event enqueueMarker() const = 0;
		virtual void enqueueBarrier() const = 0;
		virtual void retain() const = 0;
		virtual void flush() const = 0;
		virtual void finish() const = 0;
	};

	class Context
	{
	protected:
		Context() {}
		virtual ~Context() {}

	public:
		virtual size_t getReferenceCount() const = 0;
		virtual const Buffer * createBuffer(cl_mem_flags, size_t, void *) = 0;
		virtual const Program * createProgramWithSource(const std::string &) = 0;
		virtual const CommandQueue * createCommandQueue(const DeviceID &, cl_command_queue_properties) = 0;
		virtual void retain() const = 0;
		virtual void release() const = 0;

	public:
		static Context * create(ContextProperties &, const std::vector<DeviceID> &);
		static Context * createFromType(ContextProperties &, cl_device_type);

	protected:
		static void callback(const char *, const void *, size_t, void *);
	};

	class Util
	{
	public:
		static std::vector<PlatformID> getPlatformIDs();
		static void unloadCompiler();
	};
}

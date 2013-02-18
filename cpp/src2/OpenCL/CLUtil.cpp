#include <sstream>
#include <set>
#include <map>
#include "CLUtil.h"

namespace CL
{
	Exception::Exception(const char * function, int error_code)
	{
		std::ostringstream out;
		out << function << " failed with error code " << parseErrorCode(error_code);
		msg = out.str();
	}

	Exception::~Exception() throw()
	{
	}

	const char * Exception::what() const throw()
	{
		return msg.c_str();
	}

	std::string Exception::parseErrorCode(int error_code)
	{
		struct ErrorCodeData
		{
			const cl_int code;
			const char * name;
		};

#define CL_ERR(code) {code, #code}
		static ErrorCodeData error_codes[] =
		{
			CL_ERR(CL_BUILD_PROGRAM_FAILURE          ),
			CL_ERR(CL_COMPILER_NOT_AVAILABLE         ),
			CL_ERR(CL_DEVICE_NOT_AVAILABLE           ),
			CL_ERR(CL_DEVICE_NOT_FOUND               ),
			CL_ERR(CL_IMAGE_FORMAT_MISMATCH          ),
			CL_ERR(CL_IMAGE_FORMAT_NOT_SUPPORTED     ),
			CL_ERR(CL_INVALID_ARG_INDEX              ),
			CL_ERR(CL_INVALID_ARG_SIZE               ),
			CL_ERR(CL_INVALID_ARG_VALUE              ),
			CL_ERR(CL_INVALID_BINARY                 ),
			CL_ERR(CL_INVALID_BUFFER_SIZE            ),
			CL_ERR(CL_INVALID_BUILD_OPTIONS          ),
			CL_ERR(CL_INVALID_COMMAND_QUEUE          ),
			CL_ERR(CL_INVALID_CONTEXT                ),
			CL_ERR(CL_INVALID_DEVICE                 ),
			CL_ERR(CL_INVALID_DEVICE_TYPE            ),
			CL_ERR(CL_INVALID_EVENT                  ),
			CL_ERR(CL_INVALID_EVENT_WAIT_LIST        ),
			CL_ERR(CL_INVALID_GL_OBJECT              ),
			CL_ERR(CL_INVALID_GLOBAL_OFFSET          ),
			CL_ERR(CL_INVALID_HOST_PTR               ),
			CL_ERR(CL_INVALID_IMAGE_FORMAT_DESCRIPTOR),
			CL_ERR(CL_INVALID_IMAGE_SIZE             ),
			CL_ERR(CL_INVALID_KERNEL                 ),
			CL_ERR(CL_INVALID_KERNEL_ARGS            ),
			CL_ERR(CL_INVALID_KERNEL_DEFINITION      ),
			CL_ERR(CL_INVALID_KERNEL_NAME            ),
			CL_ERR(CL_INVALID_MEM_OBJECT             ),
			CL_ERR(CL_INVALID_OPERATION              ),
			CL_ERR(CL_INVALID_PLATFORM               ),
			CL_ERR(CL_INVALID_PROGRAM                ),
			CL_ERR(CL_INVALID_PROGRAM_EXECUTABLE     ),
			CL_ERR(CL_INVALID_QUEUE_PROPERTIES       ),
			CL_ERR(CL_INVALID_SAMPLER                ),
			CL_ERR(CL_INVALID_VALUE                  ),
			CL_ERR(CL_INVALID_WORK_DIMENSION         ),
			CL_ERR(CL_INVALID_WORK_GROUP_SIZE        ),
			CL_ERR(CL_INVALID_WORK_ITEM_SIZE         ),
			CL_ERR(CL_MAP_FAILURE                    ),
			CL_ERR(CL_MEM_OBJECT_ALLOCATION_FAILURE  ),
			CL_ERR(CL_MEM_COPY_OVERLAP               ),
			CL_ERR(CL_OUT_OF_HOST_MEMORY             ),
			CL_ERR(CL_OUT_OF_RESOURCES               ),
			CL_ERR(CL_PROFILING_INFO_NOT_AVAILABLE   ),
			CL_ERR(CL_SUCCESS                        ),
		};
#undef CL_ERR

		for(size_t i = 0; i < sizeof(error_codes) / sizeof(ErrorCodeData); i++)
		{
			const ErrorCodeData & error_data = error_codes[i];
			if(error_code == error_data.code)
			{
				std::ostringstream out;
				out << error_data.name;
				out << " (" << error_code << ")";
				return out.str();
			}
		}

		std::ostringstream out;
		out << "unknown error code";
		out << " (" << error_code << ")";
		return out.str();
	}


	inline void testForError(cl_int error_code, const char * function_name)
	{
		if(error_code != CL_SUCCESS)
			throw Exception(function_name, (int)error_code);
	}

	template<typename Type, typename Param, typename Enum, typename Function>
	inline Type getOpenCLValue(const Function & function, const char * function_name, Param param, Enum var)
	{
		Type value;
		cl_int error_code = function(param, var, sizeof(Type), &value, 0);
		testForError(error_code, function_name);
		return value;
	}

	template<typename Param, typename Enum, typename Function>
	inline std::string getOpenCLString(const Function & function, const char * function_name, Param param, Enum var)
	{
		size_t size;
		cl_int error_code = function(param, var, 0, 0, &size);
		testForError(error_code, function_name);

		std::vector<char> buffer(size);
		error_code = function(param, var, size, &buffer[0], 0);
		testForError(error_code, function_name);

		return std::string(&buffer[0]);
	}


	std::string DeviceID::getName() const
	{
		return getOpenCLString(::clGetDeviceInfo, "clGetDeviceInfo", device, CL_DEVICE_NAME);
	}

	std::string DeviceID::getVendor() const
	{
		return getOpenCLString(::clGetDeviceInfo, "clGetDeviceInfo", device, CL_DEVICE_VENDOR);
	}

	std::string DeviceID::getVersion() const
	{
		return getOpenCLString(::clGetDeviceInfo, "clGetDeviceInfo", device, CL_DEVICE_VERSION);
	}


	std::string PlatformID::getProfile() const
	{
		return getOpenCLString(::clGetPlatformInfo, "clGetPlatformInfo", platform, CL_PLATFORM_PROFILE);
	}

	std::string PlatformID::getVersion() const
	{
		return getOpenCLString(::clGetPlatformInfo, "clGetPlatformInfo", platform, CL_PLATFORM_VERSION);
	}

	std::string PlatformID::getName() const
	{
		return getOpenCLString(::clGetPlatformInfo, "clGetPlatformInfo", platform, CL_PLATFORM_NAME);
	}

	std::string PlatformID::getVendor() const
	{
		return getOpenCLString(::clGetPlatformInfo, "clGetPlatformInfo", platform, CL_PLATFORM_VENDOR);
	}

	std::string PlatformID::getExtensions() const
	{
		return getOpenCLString(::clGetPlatformInfo, "clGetPlatformInfo", platform, CL_PLATFORM_EXTENSIONS);
	}

	std::vector<DeviceID> PlatformID::getDeviceIDs(cl_device_type device_type)
	{
		cl_uint num_devices;
		cl_int error_code = ::clGetDeviceIDs(platform, device_type, 0, 0, &num_devices);

		if(error_code == CL_DEVICE_NOT_FOUND)
		{
			return std::vector<DeviceID>(0);
		}

		testForError(error_code, "clGetDeviceIDs");

		std::vector<cl_device_id> buffer(num_devices);
		error_code = ::clGetDeviceIDs(platform, device_type, num_devices, &buffer[0], 0);
		testForError(error_code, "clGetDeviceIDs");

		std::vector<DeviceID> devices;
		for(size_t i = 0; i < buffer.size(); i++)
			devices.push_back(DeviceID(buffer[i]));
		return devices;
	}


	ContextProperties::ContextProperties()
		: platform_defined(false)
	{
	}

	const cl_context_properties * ContextProperties::get()
	{
		data.clear();

		if(platform_defined)
		{
			data.push_back(CL_CONTEXT_PLATFORM);
			data.push_back((cl_context_properties)platform.get());
		}

		data.push_back(0);
		return &data[0];
	}

	void ContextProperties::setPlatform(const PlatformID & p)
	{
		platform_defined = true;
		platform = p;
	}


	void Event::waitFor() const
	{
		cl_event events[1] = { event };
		cl_int error_code = ::clWaitForEvents(1, events);
		testForError(error_code, "clWaitForEvents");
	}

	void Event::retain() const
	{
		cl_int error_code = ::clRetainEvent(event);
		testForError(error_code, "clRetainEvent");
	}

	void Event::release()
	{
		cl_int error_code = ::clReleaseEvent(event);
		testForError(error_code, "clReleaseEvent");
		event = 0;
	}


	class BufferImpl : public Buffer
	{
	public:
		BufferImpl(cl_mem _buffer)
			: buffer(_buffer)
		{
		}

		virtual ~BufferImpl();

		cl_mem get() const { return buffer; }

	public:
		virtual size_t getSize() const;
		virtual void * getHostPtr() const;
		virtual size_t getMapCount() const;
		virtual size_t getReferenceCount() const;
		virtual void retain() const;

	private:
		cl_mem buffer;
	};

	class KernelImpl : public Kernel
	{
	public:
		KernelImpl(cl_kernel _kernel)
			: kernel(_kernel)
		{
		}

		virtual ~KernelImpl();

		cl_kernel get() const { return kernel; }

	public:
		virtual std::string getFunctionName() const;
		virtual size_t getNumArgs() const;
		virtual size_t getReferenceCount() const;
		virtual void setArg(size_t, size_t, const void *) const;
		virtual void retain() const;

	private:
		cl_kernel kernel;
	};

	class ProgramImpl : public Program
	{
	public:
		ProgramImpl(cl_program _program)
			: program(_program)
		{
		}

		virtual ~ProgramImpl();

	public:
		virtual std::string getSource() const;
		virtual size_t getReferenceCount() const;
		virtual void build(const char *);
		virtual const Kernel * getKernel(const char *);
		virtual void retain() const;

	private:
		cl_program program;
		std::map<std::string, KernelImpl *> kernels;
	};

	class CommandQueueImpl : public CommandQueue
	{
	public:
		CommandQueueImpl(cl_command_queue _queue)
			: queue(_queue)
		{
		}

		virtual ~CommandQueueImpl();

	public:
		virtual size_t getReferenceCount() const;
		virtual Event enqueueKernel(const Kernel &, const NDRange &, const NDRange &) const;
		virtual Event enqueueTask(const Kernel &) const;
		virtual Event enqueueReadBuffer(const Buffer &, bool, size_t, size_t, void *) const;
		virtual Event enqueueWriteBuffer(const Buffer &, bool, size_t, size_t, const void *) const;
		virtual Event enqueueCopyBuffer(const Buffer &, const Buffer &, size_t, size_t, size_t) const;
		virtual Event enqueueMarker() const;
		virtual void enqueueBarrier() const;
		virtual void flush() const;
		virtual void finish() const;
		virtual void retain() const;

	private:
		cl_command_queue queue;
	};

	class ContextImpl : public Context
	{
	public:
		ContextImpl(cl_context _context)
			: context(_context)
		{
		}

		virtual ~ContextImpl();

	public:
		virtual size_t getReferenceCount() const;
		virtual const Buffer * createBuffer(cl_mem_flags, size_t, void *);
		virtual const Program * createProgramWithSource(const std::string &);
		virtual const CommandQueue * createCommandQueue(const DeviceID &, cl_command_queue_properties);
		virtual void retain() const;
		virtual void release() const;

	private:
		cl_context context;
		std::set<BufferImpl *> buffers;
		std::set<ProgramImpl *> programs;
		std::set<CommandQueueImpl *> queues;
	};


	BufferImpl::~BufferImpl()
	{
		cl_int error_code = ::clReleaseMemObject(buffer);
		testForError(error_code, "clReleaseMemObject");
	}

	size_t BufferImpl::getSize() const
	{
		return getOpenCLValue<size_t>(::clGetMemObjectInfo, "clGetMemObjectInfo", buffer, CL_MEM_SIZE);
	}

	void * BufferImpl::getHostPtr() const
	{
		return getOpenCLValue<void *>(::clGetMemObjectInfo, "clGetMemObjectInfo", buffer, CL_MEM_HOST_PTR);
	}

	size_t BufferImpl::getMapCount() const
	{
		return (size_t)getOpenCLValue<cl_uint>(::clGetMemObjectInfo, "clGetMemObjectInfo", buffer, CL_MEM_MAP_COUNT);
	}

	size_t BufferImpl::getReferenceCount() const
	{
		return (size_t)getOpenCLValue<cl_uint>(::clGetMemObjectInfo, "clGetMemObjectInfo", buffer, CL_MEM_REFERENCE_COUNT);
	}

	void BufferImpl::retain() const
	{
		cl_int error_code = ::clRetainMemObject(buffer);
		testForError(error_code, "clRetainMemObject");
	}


	KernelImpl::~KernelImpl()
	{
		cl_int error_code = ::clReleaseKernel(kernel);
		testForError(error_code, "clReleaseKernel");
	}

	std::string KernelImpl::getFunctionName() const
	{
		return getOpenCLString(::clGetKernelInfo, "clGetKernelInfo", kernel, CL_KERNEL_FUNCTION_NAME);
	}

	size_t KernelImpl::getNumArgs() const
	{
		return (size_t)getOpenCLValue<cl_uint>(::clGetKernelInfo, "clGetKernelInfo", kernel, CL_KERNEL_NUM_ARGS);
	}

	size_t KernelImpl::getReferenceCount() const
	{
		return (size_t)getOpenCLValue<cl_uint>(::clGetKernelInfo, "clGetKernelInfo", kernel, CL_KERNEL_REFERENCE_COUNT);
	}

	void KernelImpl::setArg(size_t arg_index, size_t arg_size, const void * arg_value) const
	{
		cl_int error_code = ::clSetKernelArg(kernel, (cl_uint)arg_index, arg_size, arg_value);
		testForError(error_code, "clSetKernelArg");
	}

	void KernelImpl::retain() const
	{
		cl_int error_code = ::clRetainKernel(kernel);
		testForError(error_code, "clRetainKernel");
	}


	ProgramImpl::~ProgramImpl()
	{
		for(std::map<std::string, KernelImpl *>::iterator it = kernels.begin(); it != kernels.end(); it++)
			delete it->second;

		cl_int error_code = ::clReleaseProgram(program);
		testForError(error_code, "clReleaseProgram");
	}

	std::string ProgramImpl::getSource() const
	{
		return getOpenCLString(clGetProgramInfo, "clGetProgramInfo", program, CL_PROGRAM_SOURCE);
	}

	size_t ProgramImpl::getReferenceCount() const
	{
		return (size_t)getOpenCLValue<cl_uint>(clGetProgramInfo, "clGetProgramInfo", program, CL_PROGRAM_REFERENCE_COUNT);
	}

	void ProgramImpl::build(const char * options)
	{
		cl_int error_code = ::clBuildProgram(program, 0, 0, options, 0, 0);
		testForError(error_code, "clBuildProgram");
	}

	const Kernel * ProgramImpl::getKernel(const char * kernel_name)
	{
		if(kernels.find(kernel_name) != kernels.end())
			return kernels[kernel_name];

		cl_int error_code;
		cl_kernel kernel = ::clCreateKernel(program, kernel_name, &error_code);
		testForError(error_code, "clCreateKernel");

		KernelImpl * impl = new KernelImpl(kernel);
		kernels[kernel_name] = impl;
		return impl;
	}

	void ProgramImpl::retain() const
	{
		cl_int error_code = ::clRetainProgram(program);
		testForError(error_code, "clRetainProgram");
	}


	CommandQueueImpl::~CommandQueueImpl()
	{
		cl_int error_code = ::clReleaseCommandQueue(queue);
		testForError(error_code, "clReleaseCommandQueue");
	}

	size_t CommandQueueImpl::getReferenceCount() const
	{
		return (size_t)getOpenCLValue<cl_uint>(::clGetCommandQueueInfo, "clGetCommandQueueInfo", queue, CL_QUEUE_REFERENCE_COUNT);
	}

	Event CommandQueueImpl::enqueueKernel(const Kernel & _kernel, const NDRange & global, const NDRange & local) const
	{
		if(global.dim() != local.dim())
			throw std::invalid_argument("global and local dimensions must be same");

		const KernelImpl & kernel = static_cast<const KernelImpl &>(_kernel);

		cl_event event;
		cl_int error_code = ::clEnqueueNDRangeKernel(queue, kernel.get(), global.dim(), 0, global.range(), local.range(), 0, 0, &event);
		testForError(error_code, "clEnqueueNDRangeKernel");

		return Event(event);
	}

	Event CommandQueueImpl::enqueueTask(const Kernel & _kernel) const
	{
		const KernelImpl & kernel = static_cast<const KernelImpl &>(_kernel);

		cl_event event;
		cl_int error_code = ::clEnqueueTask(queue, kernel.get(), 0, 0, &event);
		testForError(error_code, "clEnqueueTask");

		return Event(event);
	}

	Event CommandQueueImpl::enqueueReadBuffer(const Buffer & _buffer, bool blocking, size_t offset, size_t size, void * memory) const
	{
		const BufferImpl & buffer = static_cast<const BufferImpl &>(_buffer);

		cl_event event;
		cl_int error_code = ::clEnqueueReadBuffer(queue, buffer.get(), blocking ? CL_TRUE : CL_FALSE, offset, size, memory, 0, 0, &event);
		testForError(error_code, "clEnqueueReadBuffer");

		return Event(event);
	}

	Event CommandQueueImpl::enqueueWriteBuffer(const Buffer & _buffer, bool blocking, size_t offset, size_t size, const void * memory) const
	{
		const BufferImpl & buffer = static_cast<const BufferImpl &>(_buffer);

		cl_event event;
		cl_int error_code = ::clEnqueueWriteBuffer(queue, buffer.get(), blocking ? CL_TRUE : CL_FALSE, offset, size, memory, 0, 0, &event);
		testForError(error_code, "clEnqueueWriteBuffer");

		return Event(event);
	}

	Event CommandQueueImpl::enqueueCopyBuffer(const Buffer & _src, const Buffer & _dst, size_t src_offset, size_t dst_offset, size_t size) const
	{
		const BufferImpl & src = static_cast<const BufferImpl &>(_src);
		const BufferImpl & dst = static_cast<const BufferImpl &>(_dst);

		cl_event event;
		cl_int error_code = ::clEnqueueCopyBuffer(queue, src.get(), dst.get(), src_offset, dst_offset, size, 0, 0, &event);
		testForError(error_code, "clEnqueueCopyBuffer");

		return Event(event);
	}

	Event CommandQueueImpl::enqueueMarker() const
	{
		cl_event event;
		cl_int error_code = ::clEnqueueMarker(queue, &event);
		testForError(error_code, "clEnqueueMarker");

		return Event(event);
	}

	void CommandQueueImpl::enqueueBarrier() const
	{
		cl_int error_code = ::clEnqueueBarrier(queue);
		testForError(error_code, "clEnqueueBarrier");
	}

	void CommandQueueImpl::flush() const
	{
		cl_int error_code = ::clFlush(queue);
		testForError(error_code, "clFlush");
	}

	void CommandQueueImpl::finish() const
	{
		cl_int error_code = ::clFinish(queue);
		testForError(error_code, "clFinish");
	}

	void CommandQueueImpl::retain() const
	{
		cl_int error_code = ::clRetainCommandQueue(queue);
		testForError(error_code, "clRetainCommandQueue");
	}


	ContextImpl::~ContextImpl()
	{
		cl_int error_code = ::clReleaseContext(context);
		testForError(error_code, "clReleaseContext");
	}

	size_t ContextImpl::getReferenceCount() const
	{
		return (size_t)getOpenCLValue<cl_uint>(::clGetContextInfo, "clGetContextInfo", context, CL_CONTEXT_REFERENCE_COUNT);
	}

	const Buffer * ContextImpl::createBuffer(cl_mem_flags flags, size_t size, void * host_ptr)
	{
		cl_int error_code;
		cl_mem buffer = ::clCreateBuffer(context, flags, size, host_ptr, &error_code);
		testForError(error_code, "clCreateBuffer");

		BufferImpl * impl = new BufferImpl(buffer);
		buffers.insert(impl);
		return impl;
	}

	const Program * ContextImpl::createProgramWithSource(const std::string & source)
	{
		const char * src[1] = { source.c_str() };

		cl_int error_code;
		cl_program program = ::clCreateProgramWithSource(context, 1, src, 0, &error_code);
		testForError(error_code, "clCreateProgramWithSource");

		ProgramImpl * impl = new ProgramImpl(program);
		programs.insert(impl);
		return impl;
	}

	const CommandQueue * ContextImpl::createCommandQueue(const DeviceID & device, cl_command_queue_properties properties)
	{
		cl_int error_code;
		cl_command_queue queue = ::clCreateCommandQueue(context, device.get(), properties, &error_code);
		testForError(error_code, "clCreateCommandQueue");

		CommandQueueImpl * impl = new CommandQueueImpl(queue);
		queues.insert(impl);
		return impl;
	}

	void ContextImpl::retain() const
	{
		cl_int error_code = ::clRetainContext(context);
		testForError(error_code, "clRetainContext");
	}

	void ContextImpl::release() const
	{
		delete this;
	}

	Context * Context::create(ContextProperties & properties, const std::vector<DeviceID> & devices)
	{
		cl_int error_code;
		cl_context context = ::clCreateContext(properties.get(), (cl_uint)devices.size(), (const cl_device_id *)&devices[0], callback, 0, &error_code);
		testForError(error_code, "clCreateContext");

		return new ContextImpl(context);
	}

	Context * Context::createFromType(ContextProperties & properties, cl_device_type type)
	{
		cl_int error_code;
		cl_context context = ::clCreateContextFromType(properties.get(), type, callback, 0, &error_code);
		testForError(error_code, "clCreateContextFromType");

		return new ContextImpl(context);
	}

	void Context::callback(const char * errinfo, const void * private_info, size_t cb, void * user_data)
	{
	}


	std::vector<PlatformID> Util::getPlatformIDs()
	{
		cl_uint num_platforms;
		cl_int error_code = ::clGetPlatformIDs(0, 0, &num_platforms);
		testForError(error_code, "clGetPlatformIDs");

		std::vector<cl_platform_id> buffer(num_platforms);
		error_code = ::clGetPlatformIDs(num_platforms, &buffer[0], 0);
		testForError(error_code, "clGetPlatformIDs");

		std::vector<PlatformID> platforms;
		for(cl_uint i = 0; i < num_platforms; i++)
			platforms.push_back(PlatformID(buffer[i]));
		return platforms;
	}

	void Util::unloadCompiler()
	{
		cl_int error_code = ::clUnloadCompiler();
		testForError(error_code, "clUnloadCompiler");
	}
}
